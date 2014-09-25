package org.allenai.ari.solvers.graphmatch.models.features

import org.allenai.ari.solvers.graphmatch.graph.path.{ MultiNodePath, SingleNodePath, PathTrait }
import com.tinkerpop.blueprints.impls.tg.TinkerGraph

/** This is basically a negative bias against all matches that should be outweighted by
  * positive feature in the case of a good quality match.
  * Created by TomK on 7/28/14.
  */

object NodePathDeletionFeature extends FeatureWeightPairTrait {

  override val featureTag: String = "NodeDel"

  weight = -0.1

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double = {
    val qs = qp match {
      case p: SingleNodePath => 1.0
      case p: MultiNodePath => 1.0 * p.containedNodes.size
      case _ => 0.0
    }
    val es = ep match {
      case p: SingleNodePath => 1.0

      case p: MultiNodePath => 1.0
      case _ => 0.0
    }
    (qs + es) * 0.5
  }
}
