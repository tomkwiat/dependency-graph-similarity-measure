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

import org.allenai.ari.solvers.graphmatch.graph.path.{ MultiNodePath, SingleNodePath, PathTrait }
import com.tinkerpop.blueprints.impls.tg.TinkerGraph

/** This is basically a negative bias against all matches that should be outweighted by
  * positive feature in the case of a good quality match.
  * Created by TomK on 7/28/14.
  */

object NodePathDeletionFeature extends FeatureWeightPairTrait {

  override val featureTag: String = "NodeDel"

  weight = -0.1

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double = {
    val qs = qp match {
      case p: SingleNodePath => 1.0
      case p: MultiNodePath => 1.0 * p.containedNodes.size
      case _ => 0.0
    }
    val es = ep match {
      case p: SingleNodePath => 1.0

      case p: MultiNodePath => 1.0
      case _ => 0.0
    }
    (qs + es) * 0.5
  }
}
