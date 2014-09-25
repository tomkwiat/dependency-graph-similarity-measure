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
