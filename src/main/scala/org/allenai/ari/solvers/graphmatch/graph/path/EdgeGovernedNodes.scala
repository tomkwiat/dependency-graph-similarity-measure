package org.allenai.ari.solvers.graphmatch.graph.path

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.{ Direction, Edge, Element, Vertex }
import scala.collection.immutable

import scala.collection.JavaConverters._

/** This allows us to keep track of deleted subgraphs by storing the nodes under a
  * given edge.
  *
  */
case class EdgeGovernedNodes(val graph: TinkerGraph, val root: Vertex) {

  val egN = getGovernedNodes(root)

  def apply(edge: Edge): Set[Vertex] = {
    egN(edge)
  }

  def getGovernedNodes(element: Element): immutable.Map[Element, Set[Vertex]] = {

    element match {
      case e: Edge => {
        val vm = getGovernedNodes(e.getVertex(Direction.IN))
        val vm2 = vm.+(e -> vm(e.getVertex(Direction.IN)))
        vm2
      }
      case n: Vertex => {
        val em = n.getEdges(Direction.OUT).asScala.flatMap(e => getGovernedNodes(e))
        val nn = em.flatMap(e => e._2).toSet.+(n)
        Map(n -> nn).++(n.getEdges(Direction.OUT).asScala.flatMap(e => getGovernedNodes(e)))
      }
      case _ => Map.empty
    }

  }

}
