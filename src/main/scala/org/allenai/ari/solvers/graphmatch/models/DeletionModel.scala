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
