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
