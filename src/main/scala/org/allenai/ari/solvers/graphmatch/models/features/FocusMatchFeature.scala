/**************************************************************************

   Copyright 2014 Allen Institute for Artificial Intelligence Foundation

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

****************************************************************************/


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
