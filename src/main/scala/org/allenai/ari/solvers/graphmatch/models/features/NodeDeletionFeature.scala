package org.allenai.ari.solvers.graphmatch.models.features

import com.tinkerpop.blueprints.Vertex

/** As implemented this is just a bias against all nodes. It will actually encourage collapsing. Weird???
  * Created by TomK on 7/28/14.
  */
object NodeDeletionFeature extends DeletionFeatureWeightPairTrait {

  override val featureTag: String = "SUBTREE-DEL"

  weight = -0.5

  override def feature(node: Vertex): Double = {
    1.0
  }

}
