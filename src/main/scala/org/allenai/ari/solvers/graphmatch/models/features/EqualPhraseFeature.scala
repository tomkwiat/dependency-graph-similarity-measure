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

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.Vertex

import org.allenai.ari.solvers.graphmatch.graph.path._

import org.allenai.ari.solvers.graphmatch.tools._

/** Check that the
  */
object EqualPhraseFeature extends LightweightLexicalFeatureTrait {

  weight = 3.0

  override val featureTag: String = "EqualPhrase"
  override val requireStemming = false

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double = {

    qp match {
      case p: EdgePath => return 0.0
      case _ => {}
    }
    ep match {
      case p: EdgePath => return 0.0
      case _ => {}
    }

    val qw: Seq[String] = PathToPhraseSeq(qp)
    val ew: Seq[String] = PathToPhraseSeq(ep)

    wordsFeature(qw, ew)

  }

  def generatePhrase(nodes: Seq[Vertex]): String = {
    val pairs = nodes.map(n => (n.getProperty("text"): String, n.getProperty("position"): Int)) //
    val pairsSorted = pairs.sortBy[Int](_._2)
    val s = pairsSorted.map(p => p._1).reduce(_ + " " + _)
    s
  }

  override def wordsFeature(words1: Seq[String], words2: Seq[String], focus: Option[String]): Double = {

    val w1 = words1.filter(w => !StopWord(w))
    val w2 = words2.filter(w => !StopWord(w))

    if (w1.size > 0 && w1.equals(w2)) {
      1.0
    } else {
      0.0
    }
  }
}
