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

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import org.allenai.ari.solvers.graphmatch.models.features.{ LightweightLexicalFeatureTrait, FeatureWeightPairTrait }
import org.allenai.ari.solvers.graphmatch.graph.path.PathPair

/** Created by TomK on 7/14/14.
  */
case class Model(val features: Seq[FeatureWeightPairTrait]) {

  val weights = collection.mutable.HashMap[String, Double](features.map(f => f.featureTag -> f.weight).toSeq: _*)

  def score(pathPair: PathPair, evidenceGraph: TinkerGraph, focus: Option[String] = None): Double = {
    features.map(f =>
      f.feature(pathPair.questionPath, pathPair.evidencePath, evidenceGraph, focus) * weights.getOrElse(f.featureTag, 0.0)).sum
  }

  def updateWeight(feature: FeatureWeightPairTrait, update: Double) {
    weights.put(feature.featureTag, weights.getOrElse(feature.featureTag, 0.0) + update)
  }

  def lightWeightLexScore(words1: Seq[String], words2: Seq[String], focus: Option[String]): Double = {
    features.map(f => f match {
      case f: LightweightLexicalFeatureTrait => f.scoreWords(words1, words2, focus)
      case _ => 0.0
    }).sum
  }

}

