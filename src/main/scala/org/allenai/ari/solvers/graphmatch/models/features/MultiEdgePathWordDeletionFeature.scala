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


package org.allenai.ari.solvers.graphmatch.models.features

import org.allenai.ari.solvers.graphmatch.graph.path.{ MultiEdgePath, PathTrait }
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.Vertex

/** Created by TomK on 7/25/14.
  */
object MultiEdgePathWordDeletionFeature extends FeatureWeightPairTrait {

  override val featureTag: String = "EDGEWORDDEL"

  weight = -0.1

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double = {

    val qf: Double = qp match {
      case q: MultiEdgePath => q.containedNodes.map(n => nodeDelFeat(n)).sum
      case _ => 0.0
    }

    val ef: Double = ep match {
      case e: MultiEdgePath => e.containedNodes.map(n => nodeDelFeat(n)).sum
      case _ => 0.0
    }

    (qf + ef) * 0.5

  }

  def nodeDelFeat(n: Vertex): Double = {
    1.0
  }

}
