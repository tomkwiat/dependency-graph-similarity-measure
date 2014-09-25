package org.allenai.ari.solvers.graphmatch.models.features

import org.allenai.ari.solvers.graphmatch.graph.path.PathTrait
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import org.allenai.ari.solvers.graphmatch.tools.PathToPhraseSeq

/** Check whether the focus is in BOTH matched paths.
  *
  */

object FocusMatchFeature extends FeatureWeightPairTrait {

  override val featureTag: String = "FOCUSMATCH"

  weight = 1.0

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double = {

    val qw = PathToPhraseSeq(qp).map(w => w.toLowerCase)
    val ew = PathToPhraseSeq(ep).map(w => w.toLowerCase)

    wordsFeature(qw, ew, focus)

  }

  def wordsFeature(words1: Seq[String], words2: Seq[String], focus: Option[String]): Double = {

    focus match {

      case Some(f) => {
        val focusWords = f.toLowerCase().split(" ")
        if (words1.containsSlice(focusWords) && words2.containsSlice(focusWords)) {
          1.0
        } else {
          0.0
        }
      }

      case _ => 0.0

    }

  }
}
