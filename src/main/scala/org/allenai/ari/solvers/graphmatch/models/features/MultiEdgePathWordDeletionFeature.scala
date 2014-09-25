package org.allenai.ari.solvers.graphmatch.models.features

import org.allenai.ari.solvers.graphmatch.graph.path.{ MultiEdgePath, PathTrait }
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.Vertex

/** Created by TomK on 7/25/14.
  */
object MultiEdgePathWordDeletionFeature extends FeatureWeightPairTrait {

  override val featureTag: String = "EDGEWORDDEL"

  weight = -0.1

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double = {

    val qf: Double = qp match {
      case q: MultiEdgePath => q.containedNodes.map(n => nodeDelFeat(n)).sum
      case _ => 0.0
    }

    val ef: Double = ep match {
      case e: MultiEdgePath => e.containedNodes.map(n => nodeDelFeat(n)).sum
      case _ => 0.0
    }

    (qf + ef) * 0.5

  }

  def nodeDelFeat(n: Vertex): Double = {
    1.0
  }

}
