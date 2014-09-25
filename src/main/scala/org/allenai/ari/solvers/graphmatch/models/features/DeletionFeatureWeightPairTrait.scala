package org.allenai.ari.solvers.graphmatch.models.features

import com.tinkerpop.blueprints.Vertex

/** Deletion features signal the deletion of a node path from a graph.
  */
trait DeletionFeatureWeightPairTrait {

  val featureTag: String

  var weight = 0.0

  def feature(node: Vertex): Double

}
