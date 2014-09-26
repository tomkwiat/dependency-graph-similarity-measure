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


package org.allenai.ari.solvers.graphmatch.models

import com.tinkerpop.blueprints.Vertex
import org.allenai.ari.solvers.graphmatch.models.features.DeletionFeatureWeightPairTrait

/** Created by TomK on 7/28/14.
  */
case class DeletionModel(val features: Seq[DeletionFeatureWeightPairTrait]) {

  def score(node: Vertex): Double = {

    features.map(f => f.feature(node) * f.weight).sum

  }
}
