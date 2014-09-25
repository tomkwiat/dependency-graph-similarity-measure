package org.allenai.ari.solvers.graphmatch.graph.path

import com.tinkerpop.blueprints.{ Edge, Direction, Vertex }
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import scala.collection.mutable

import collection.JavaConversions._
// Required to avoid "filter not defined for ..." bug:
//  c.f. http://stackoverflow.com/questions/24960835/scala-java-error-value-filter-is-not-a-member-of-java-util-map-works-outside-o
// should probably fix this..

case class SentencePaths(val rootNode: Vertex, val graph: TinkerGraph) {

  val nodePathChart = new mutable.HashMap[Vertex, mutable.Set[PathTrait]]
  val edgePathChart = new mutable.HashMap[Edge, mutable.Set[PathTrait]]

  // Assume Tree.
  def getPath(startNode: Vertex, endNode: Vertex): Option[PathTrait] = {
    val s: mutable.Set[PathTrait] = nodePathChart(startNode)
    for (p <- s) {
      p match {
        case p: SingleNodePath => {
          if (p.inNode.equals(startNode) && p.outNode.equals(endNode)) {
            return Some(p)

          }
        }
        case p: MultiNodePath => {
          if (p.inNode.equals(startNode) && p.outNode.equals(endNode)) {
            return Some(p)
          }
        }
        case _ => throw new Exception("Not found Node path :: WTF?")
      }
    }
    return None
  }

  def addPaths(node: Vertex) {

    val nodePaths = new mutable.HashSet[PathTrait]() // Going to create all paths headed by this node.
    nodePaths.+=(new SingleNodePath(node)) // Including this node itself.

    for (topEdge: Edge <- node.getEdges(Direction.OUT)) { // For each of the outgoing edges.

      val edgePaths = new mutable.HashSet[PathTrait]() // Going to create all paths headed by that edge.

      edgePaths.+=(new SingleEdgePath(topEdge)) // Including the edge itself.

      // Recurse on the end node of the edge.
      val child = topEdge.getVertex(Direction.IN)
      addPaths(child)

      // Find all of the paths governed by that node.
      nodePathChart(child).foreach(
        path => path match {
          case path: SingleNodePath => {
            nodePaths.+=(new MultiNodePath(node, path.outNode, Seq(node, path.outNode)))
          }
          case path: MultiNodePath => {
            nodePaths.+=(new MultiNodePath(node, path.outNode, path.containedNodes.+:(node)))
          }
          case _ => throw new Exception("Not found Edge path :: WTF?")
        })

      // And all the edge paths governed by that node.
      for (e <- child.getEdges(Direction.OUT)) {
        edgePathChart(e).foreach(
          path => path match {
            case path: SingleEdgePath => edgePaths.+=(new MultiEdgePath(topEdge, path.outEdge, Seq(topEdge.getVertex(Direction.OUT))))
            case path: MultiEdgePath => edgePaths.+=(new MultiEdgePath(topEdge, path.outEdge, path.containedNodes.+:(topEdge.getVertex(Direction.OUT))))
            case _ => throw new Exception("asdfj;lkasasdfasfdnfd")
          })
      }
      edgePathChart.put(topEdge, edgePaths)
    }
    nodePathChart.put(node, nodePaths) // NODE PATH SHOULD KNOW WHAT CHILDREN THERE ARE!!!!
  }

  override def toString: String = {
    nodePathChart.toString() + edgePathChart.toString()

  }
}
