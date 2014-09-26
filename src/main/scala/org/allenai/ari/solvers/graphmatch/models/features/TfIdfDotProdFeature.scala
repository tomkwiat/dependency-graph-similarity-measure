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

import org.allenai.ari.solvers.graphmatch.graph.path.{ EdgePath, PathTrait }
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import breeze.util.HashIndex
import breeze.linalg._
import edu.knowitall.tool.stem.MorphaStemmer
import org.allenai.ari.solvers.graphmatch.tools.PathToPhraseSeq.PathToStemmedPhraseSeq

/** This should prefer the rarer words. No norm.
  * Created by TomK on 8/26/14.
  */
object TfIdfDotProdFeature extends FeatureWeightPairTrait {

  override val featureTag: String = "TfIdfEq"

  val stemmer = new MorphaStemmer()

  weight = 0.01

  val barronsFile = "/Users/TomK/Projects/Ari/data/barrons-4th-grade/Barrons-Grade-4-Science-sentences-unnumbered.txt"
  val barronsCorpus = scala.io.Source.fromFile(barronsFile)("UTF-8").getLines().toSeq

  // We can assume that anything not in the Barron's corpus does not matter because why would it?
  val idf = barronsCorpus.flatMap(l => l.split("\\W+").map(w => stemmer.stem(w.toLowerCase)))
    .groupBy((word: String) => word)
    .mapValues(v => math.log(barronsCorpus.size.toDouble / (1 + v.length)))

  val smoothVal = math.log(barronsCorpus.size.toDouble)

  val vocab = idf.keys.toSeq

  def wordsFeature(words1: Seq[String], words2: Seq[String], focus: Option[String]): Double = {

    // Build vector representations of the two phrases.
    val vocabIndex = new HashIndex[String]()

    // More loops than needed but these should be small loops.
    val indexAndIdf1: Seq[(Int, Double)] =
      words1.map(w => (vocabIndex.index(w.toLowerCase), idf.getOrElse(w, smoothVal)))
    val indexAndIdf2: Seq[(Int, Double)] =
      words2.map(w => (vocabIndex.index(w.toLowerCase), idf.getOrElse(w, smoothVal)))
    val indexAndIdf2Sorted = indexAndIdf2.sortBy(_._1) // Indices need to be ordered.

    val vector1 = new SparseVector[Double](indexAndIdf1.map(p => p._1).toArray,
      indexAndIdf1.map(p => p._2).toArray, vocabIndex.size)
    val vector2 = new SparseVector[Double](indexAndIdf2Sorted.map(p => p._1).toArray,
      indexAndIdf2Sorted.map(p => p._2).toArray, vocabIndex.size)

    vector1 dot vector2

  }

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String]): Double = {

    qp match {
      case p: EdgePath => return 0.0
      case _ => {}
    }
    ep match {
      case p: EdgePath => return 0.0
      case _ => {}
    }

    val qw = PathToStemmedPhraseSeq(qp)
    val ew = PathToStemmedPhraseSeq(ep)

    wordsFeature(qw, ew, focus)
  }

}

