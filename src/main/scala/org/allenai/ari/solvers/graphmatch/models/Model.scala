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

