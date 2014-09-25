package org.allenai.ari.solvers.graphmatch

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.{ Element, Vertex, Edge, Direction }

import scala.collection.mutable

import scala.collection.JavaConversions._
import org.allenai.ari.solvers.graphmatch.graph.path._
import org.allenai.ari.solvers.graphmatch.models._
import org.allenai.ari.solvers.graphmatch.models.DeletionModel


/** Created by TomK on 7/14/14.
  */
case class GraphMatcher(val questionPaths: SentencePaths, val questionGraph: TinkerGraph,
    val focus: String,
    val evidencePaths: SentencePaths, val evidenceGraph: TinkerGraph,
    val matchModel: Model, val deletionModel: DeletionModel) {


  val questionRoot = questionGraph.getVertex("ROOT-0")

  val evidenceRoot = evidenceGraph.getVertex("ROOT-0")

  // First of all, add in all possible path pairs and index them by their heads.
  val headHeadIndexedPathPairs = new mutable.HashMap[(Element, Element), Seq[PathPair]]

  // Then score each pathPair and store the best one for each head pair.
  val headHeadIndexedMaxPathPairs = new mutable.HashMap[(Element, Element), (PathPair, Double)]

  // Also need to know which edges go within each other when coming out of a node.
  val outgoingEdgePairingsForVertexPair = new mutable.HashMap[(Vertex, Vertex), Seq[(Edge, Edge)]]()

  val edgeGovernedNodes = EdgeGovernedNodes(questionGraph, questionRoot)

  def matchVertices(questionVertex: Vertex, evidenceVertex: Vertex) {

    if (headHeadIndexedMaxPathPairs.contains((questionVertex, evidenceVertex))) return

    var maxPathPair: PathPair = null
    var maxPathPairScore = Double.NegativeInfinity
    var maxOutgoingEdgePairMatches = Seq[(Edge, Edge)]()

    // For each of the edges that comes out of the path pair, we want to find the highest scoring match. It doesn't
    // matter if the same evidence path gets used multiple times. There is no way to reasonably get around this.

    for (pathPair: PathPair <- headHeadIndexedPathPairs.getOrElse((questionVertex, evidenceVertex), Seq.empty)) {

      val questionEdges: Seq[Edge] = pathPair.questionPath match {
        case qp: SingleNodePath => qp.outNode.getEdges(Direction.OUT).toSeq
        case qp: MultiNodePath => qp.containedNodes.flatMap(n => n.getEdges(Direction.OUT)).toSeq.
          filter(e => !qp.containedNodes.contains(e.getVertex(Direction.IN)))
        case _ => throw new Exception("not a node path")
      }

      val evidenceEdges: Seq[Edge] = pathPair.evidencePath match {
        case ep: SingleNodePath => ep.outNode.getEdges(Direction.OUT).toSeq
        case ep: MultiNodePath => ep.containedNodes.flatMap(n => n.getEdges(Direction.OUT)).toSeq.
          filter(e => !ep.containedNodes.contains(e.getVertex(Direction.IN)))
        case _ => throw new Exception("not a node path")
      }

      // Initialise a Map that will hold the best match for each question edge.
      val questionEdgeMatches = new mutable.HashMap[Edge, (Edge, Double)]

      for (e: Edge <- questionEdges) {
        questionEdgeMatches.put(e, (null, calculateDeletionScore(e)))
      }

      for (questionEdge: Edge <- questionEdges; evidenceEdge: Edge <- evidenceEdges) {

        matchEdges(questionEdge, evidenceEdge)

        val edgePathScore: Double = headHeadIndexedMaxPathPairs((questionEdge, evidenceEdge))._2

        if (edgePathScore > questionEdgeMatches(questionEdge)._2) {
          questionEdgeMatches.put(questionEdge, (evidenceEdge, edgePathScore))
        }

      }

      val childrenScores = questionEdgeMatches.foldLeft(0.0)(_ + _._2._2)

      pathPair.localScore = matchModel.score(pathPair, evidenceGraph, Some(focus))

      val pathScore = pathPair.localScore + childrenScores

      if (pathScore > maxPathPairScore) {
        maxPathPairScore = pathScore
        maxPathPair = pathPair
        maxOutgoingEdgePairMatches = questionEdgeMatches.map(f => (f._1, f._2._1)).toSeq
      }

    }

    headHeadIndexedMaxPathPairs.put((questionVertex, evidenceVertex), (maxPathPair, maxPathPairScore))

    outgoingEdgePairingsForVertexPair.put((questionVertex, evidenceVertex), maxOutgoingEdgePairMatches)

  }

  def matchEdges(questionEdge: Edge, evidenceEdge: Edge) {

    var maxPathPair: PathPair = null
    var maxPathPairScore = Double.NegativeInfinity

    if (headHeadIndexedMaxPathPairs.contains((questionEdge, evidenceEdge))) return

    for (pathPair: PathPair <- headHeadIndexedPathPairs.getOrElse((questionEdge, evidenceEdge), Seq.empty)) {
      pathPair.tails match {
        case (qe: Edge, ee: Edge) => {

          matchVertices(qe.getVertex(Direction.IN), ee.getVertex(Direction.IN))

          val nextMatchScore: Double = headHeadIndexedMaxPathPairs.
            getOrElse((qe.getVertex(Direction.IN), ee.getVertex(Direction.IN)), (null, Double.NegativeInfinity))._2

          // We don't have to match the edge, we can also delete it.
          val nextDeletionScore: Double = calculateDeletionScore(qe) // Check this!

          pathPair.localScore = matchModel.score(pathPair, evidenceGraph)

          val pathPairScore = scala.math.max(nextMatchScore, nextDeletionScore) + pathPair.localScore

          if (pathPairScore > maxPathPairScore) {
            maxPathPairScore = pathPairScore
            maxPathPair = pathPair
          }

        }
        case _ => throw new Exception(s"Somehow tail are not edges: ${pathPair.tails}")
      }
    }

    headHeadIndexedMaxPathPairs.put((questionEdge, evidenceEdge), (maxPathPair, maxPathPairScore))

  }

  // TODO - implement scoring function for deletion of entire sub-graph.
  def calculateDeletionScore(questionEdge: Edge): Double = {
    edgeGovernedNodes(questionEdge).map(n => deletionModel.score(n)).sum
  }

  def initializeAllPathPathMatches() {

    for (qPaths <- questionPaths.nodePathChart; ePaths <- evidencePaths.nodePathChart) {
      headHeadIndexedPathPairs.put((qPaths._1, ePaths._1),
        { for (qPath <- qPaths._2; ePath <- ePaths._2) yield PathPair(qPath, ePath) }.toSeq)
    }

    for (qPaths <- questionPaths.edgePathChart; ePaths <- evidencePaths.edgePathChart) {
      headHeadIndexedPathPairs.put((qPaths._1, ePaths._1),
        { for (qPath <- qPaths._2; ePath <- ePaths._2) yield PathPair(qPath, ePath) }.toSeq)
    }

  }

  def bestPathVisualizable: String = {
    val g = new TinkerGraph()

    bestPathFromNode(questionRoot, evidenceRoot, g)

    val nodes = g.getVertices().toSeq // .sortBy(n => n.getProperty("position") : Int).reverse
    var i = 1
    for (n <- nodes) {
      n.setProperty("position", i)
      i += 1
    }

    if (g.getEdges.isEmpty) {
      return ""
      //      throw new Exception(s"No edges for ${GraphToPhrase(g)}")
    }

    g.getEdges.map(e =>
      s"${e.getProperty("ppname")}(${e.getVertex(Direction.OUT).getProperty("ppname")}-${e.getVertex(Direction.OUT).getProperty("position")}, " +
        s"${e.getVertex(Direction.IN).getProperty("ppname")}-${e.getVertex(Direction.IN).getProperty("position")})").reduce(_ + "\n" + _)
  }

  def bestPathFromNode(qv: Vertex, ev: Vertex, graph: TinkerGraph): Vertex = {

    val pp = headHeadIndexedMaxPathPairs((qv, ev))

    val n = graph.getVertex(pp.toString) match {
      case null => {
        val n = graph.addVertex(pp.toString)
        n.setProperty("ppname", pp._1.toString)
        n.setProperty("score", pp._2)
        n
      }
      case n: Vertex => n
    }

    outgoingEdgePairingsForVertexPair.get((qv, ev)) match {
      case Some(s: Seq[(Edge, Edge)]) => {
        s.foreach(ep => {
          bestPathFromEdge(ep._1, ep._2, n, graph)
        })
        n
      }
      case None => n
    }
  }

  def bestPathFromEdge(qe: Edge, ee: Edge, pn: Vertex, graph: TinkerGraph) {

    //    logger.debug(s"edging it $qe ${qe.getVertex(Direction.IN)} ; ${qe.getVertex(Direction.OUT)}")

    ee match {
      case null => {
        //        graph.addEdge(null, pn, qe.getVertex(Direction.IN), qe.getLabel)
        qe.getVertex(Direction.IN).getEdges(Direction.OUT).foreach(e => bestPathFromEdge(e, null, qe.getVertex(Direction.IN), graph))
      }

      case ee: Edge => {
        val pp = headHeadIndexedMaxPathPairs((qe, ee))
        pp._1.tails match {
          case (t1: Edge, t2: Edge) => {
            val e = graph.addEdge(null, pn, bestPathFromNode(t1.getVertex(Direction.IN), t2.getVertex(Direction.IN), graph), pp.toString)
            e.setProperty("pp", pp)
            e.setProperty("ppname", pp._1.toString)
          }
          case _ => throw new Exception("Non edge tails for tail path")
        }
      }

    }
  }

  def bestPathToString(questionElement: Element, evidenceElement: Element, parentNodeName: Option[String], printScore: Boolean = true): String = {

    (questionElement, evidenceElement, parentNodeName) match {

      case (qe: Vertex, ee: Vertex, pn: Any) => {
        val pp = headHeadIndexedMaxPathPairs((questionElement, evidenceElement))
        val localString = pp._1.toString + { if (printScore) { "::" + pp._1.localScore } else "" }
        outgoingEdgePairingsForVertexPair.get((qe, ee)) match {
          case Some(s: Seq[(Edge, Edge)]) => localString + ")\n" +
            s.map(ep => bestPathToString(ep._1, ep._2, Some(localString), printScore)).foldLeft("")((s, c) => s + c)
          case None => localString + ")"
        }
      }

      case (qe: Edge, ee: Edge, Some(pns)) => {
        val pp = headHeadIndexedMaxPathPairs((questionElement, evidenceElement))
        val localString = pp._1.toString + { if (printScore) { "::" + pp._1.localScore } else "" }
        pp._1.tails match {
          case (qt: Edge, et: Edge) => s"$localString($pns,${bestPathToString(qt.getVertex(Direction.IN), et.getVertex(Direction.IN), None, printScore)}".toString
          case _ => throw new Exception("Non edge tails for tail path")
        }
      }

      case (qe: Edge, null, Some(pns)) => {
        val qn = qe.getId.toString
        s"$qn($pns,${dependencyTreeString(qe.getVertex(Direction.IN))}"
      }

      case _ => {
        throw new Exception(s"Unprintable format")
      }

    }
  }

  def dependencyTreeString(root: Vertex): String = {
    val localString = root.getId
    val edgesString: String = if (root.getEdges(Direction.OUT).isEmpty) "" else
      root.getEdges(Direction.OUT).map(e => e.getProperty("label").toString + "(" + localString + "," + dependencyTreeString(e.getVertex(Direction.IN))).reduce(_ + " " + _)
    localString + ")\n" + edgesString
  }

}
