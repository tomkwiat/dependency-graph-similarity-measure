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

/** Lightweight lexical features are cheap to calculate and are used to filter
  * sentences with the BoW scorer before running the expensive GraphMatch scorer.
  */
trait LightweightLexicalFeatureTrait extends FeatureWeightPairTrait {

  val requireStemming: Boolean

  def wordsFeature(w1: Seq[String], w2: Seq[String], focus: Option[String] = None): Double

  def scoreWords(words1: Seq[String], words2: Seq[String], focus: Option[String] = None): Double = {
    wordsFeature(words1, words2, focus) * weight
  }

}
