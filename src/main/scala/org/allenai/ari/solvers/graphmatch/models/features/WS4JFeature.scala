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

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import org.allenai.ari.solvers.graphmatch.graph.path.PathTrait

/** Created by TomK on 7/15/14.
  */
object WS4JFeature extends FeatureWeightPairTrait {

  override val featureTag: String = "WS4J"

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double = {
    0.0
  }
}
