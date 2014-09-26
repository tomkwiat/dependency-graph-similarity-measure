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

/** A path can be an EdgePath (bookended by edge(s)) or a NodePath (bookended by node(s)).
  *
  * A path pair holds two paths from different parses and assigns this match a score.
  */

case class PathPair(val questionPath: PathTrait, val evidencePath: PathTrait) {

  var localScore = Double.NegativeInfinity

  val (heads, tails) = (questionPath, evidencePath) match {
    case (qp: NodePath, ep: NodePath) => ((qp.inNode, ep.inNode), (qp.outNode, ep.outNode))
    case (qp: EdgePath, ep: EdgePath) => ((qp.inEdge, ep.inEdge), (qp.outEdge, ep.outEdge))
    case (qp: NullNodePath, ep: Any) => ((null, null), (null, null)) // Go on, use options
    case (qp: NullEdgePath, ep: Any) => ((null, null), (null, null))
    case _ => throw new Exception(s"Unknown path type for either or both of $questionPath, $evidencePath")
  }

  override def toString: String = {
    (questionPath, evidencePath) match {
      case (qe: NodePath, ee: NodePath) => s"${qe.toString}::${ee.toString}"
      case (qe: EdgePath, ee: EdgePath) => s"<${qe.toString}::${ee.toString}>"
      case (qp: NullNodePath, ep: Any) => "NULL NODE" // Go on, use options
      case (qp: NullEdgePath, ep: Any) => "NULL EDGE"
      case _ => throw new Exception(s"Unallowed path heads $questionPath, $evidencePath")
    }
  }
}
