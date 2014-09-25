package org.allenai.ari.solvers.graphmatch.models.features

import org.allenai.ari.solvers.graphmatch.graph.path._

import com.tinkerpop.blueprints.impls.tg.TinkerGraph

/** feature fires on a pair of matched paths to signal matches.
  *
  */
trait FeatureWeightPairTrait {

  val featureTag: String

  var weight = 0.0

  def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double

  def score(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double = {
    feature(qp, ep, graph, focus) * weight
  }

}

