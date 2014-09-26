/**************************************************************************

   Copyright 2014 Allen Institute for Artificial Intelligence Foundation

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

****************************************************************************/


package org.allenai.ari.solvers.graphmatch.graph.path

import com.tinkerpop.blueprints.{ Vertex, Edge }

/** NodePath is bookended by nodes :
  * MultiNodePath   :: o-...-o
  * SingleNodePath  :: o
  *
  * EdgePath is bookended by edges :
  * MultiEdgePath   ::  -o-...-o-
  * SingleEdgePath  ::  -
  *
  * NullEdgePath can match an EdgePath to mimic EdgePath deletion
  *
  * NullNodePath can match a NodePath to mimic NodePath deletion
  *
  */

trait PathTrait {}

trait EdgePath extends PathTrait {
  val inEdge: Edge
  val outEdge: Edge
}

trait NodePath extends PathTrait {
  val outNode: Vertex
  val inNode: Vertex
}

case class SingleEdgePath(val edge: Edge) extends EdgePath {
  val inEdge = edge
  val outEdge = edge
  override val toString = inEdge.getLabel

}

case class SingleNodePath(val node: Vertex) extends NodePath {
  val inNode = node
  val outNode = node
  override val toString = inNode.getProperty("text").toString
}

case class MultiEdgePath(val inEdge: Edge, val outEdge: Edge, val containedNodes: Seq[Vertex]) extends EdgePath {
  override val toString = inEdge.getLabel + "#" + outEdge.getLabel
}

case class MultiNodePath(val inNode: Vertex, val outNode: Vertex, val containedNodes: Seq[Vertex]) extends NodePath {
  override val toString: String = {
    containedNodes.map(v =>
      {
        val bits = v.getId.toString.split("-")
        val word = bits.dropRight(1)
        val position = bits.last.toInt
        (word, position)
      }).sortBy(_._2).flatMap(e => e._1).reduce((s, c) => s + "_" + c)
  }
}

// Not sure if these need anything more.
case class NullNodePath(node: Vertex) extends PathTrait {}

case class NullEdgePath(edge: Edge) extends PathTrait {}

