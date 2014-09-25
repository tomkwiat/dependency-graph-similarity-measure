package org.allenai.ari.solvers.graphmatch.models.features

import org.allenai.ari.solvers.graphmatch.graph.path.{ EdgePath, PathTrait }
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import breeze.util.HashIndex
import org.allenai.ari.solvers.graphmatch.tools.{ PathToPhraseSeq, StopWord }
import breeze.linalg._
import org.allenai.ari.solvers.graphmatch.tools.PathToPhraseSeq.PathToStemmedPhraseSeq
import edu.knowitall.tool.stem.MorphaStemmer

/** Created by TomK on 8/25/14.
  */
object TfIdfCosineFeature extends LightweightLexicalFeatureTrait {

  override val featureTag: String = "TfIdfEq"
  override val requireStemming = true

  val stemmer = new MorphaStemmer()

  weight = 1.0

  val barronsFile = "/Users/TomK/Projects/Ari/data/barrons-4th-grade/Barrons-Grade-4-Science-sentences-unnumbered.txt"
  val barronsCorpus = scala.io.Source.fromFile(barronsFile)("UTF-8").getLines().toSeq

  // We can assume that anything not in the Barron's corpus does not matter because why would it?
  val idf = barronsCorpus.flatMap(l => l.split("\\W+").map(w => stemmer.stem(w.toLowerCase)))
    .groupBy((word: String) => word)
    .mapValues(v => math.log(barronsCorpus.size.toDouble / (1 + v.length)))

  val smoothVal = math.log(barronsCorpus.size.toDouble)

  val vocab = idf.keys.toSeq

  override def wordsFeature(words1: Seq[String], words2: Seq[String], focus: Option[String]): Double = {

    // Build vector representations of the two phrases.
    val vocabIndex = new HashIndex[String]()

    // More loops than needed but these should be small loops.
    // Actually, these are causing some sort of hellish slowdown, use TreeMap to allow
    // more efficient aggregation and sorting.
    val indexAndIdf1: Seq[(Int, Double)] =
      words1.map(w => w.toLowerCase).filter(w => !StopWord(w)).map(w => (vocabIndex.index(w), idf.getOrElse(w, smoothVal))).groupBy(_._1).mapValues { _.unzip._2.sum }.toSeq

    val indexAndIdf2: Seq[(Int, Double)] =
      words2.map(w => w.toLowerCase).filter(w => !StopWord(w)).map(w => (vocabIndex.index(w), idf.getOrElse(w, smoothVal))).groupBy(_._1).mapValues { _.unzip._2.sum }.toSeq

    val indexAndIdf1Sorted = indexAndIdf1.sortBy(_._1)
    val indexAndIdf2Sorted = indexAndIdf2.sortBy(_._1)

    val vector1: SparseVector[Double] = new SparseVector[Double](indexAndIdf1Sorted.map(p => p._1).toArray, indexAndIdf1Sorted.map(p => p._2).toArray, vocabIndex.size)

    val vector2: SparseVector[Double] = new SparseVector[Double](indexAndIdf2Sorted.map(p => p._1).toArray, indexAndIdf2Sorted.map(p => p._2).toArray, vocabIndex.size)

    val denom: Double = (norm(vector1) * norm(vector2))
    val featureVal = if (denom > 0.0) ((vector1 dot vector2) / denom) else 0.0
    featureVal

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

    wordsFeature(qw, ew)
  }

}
