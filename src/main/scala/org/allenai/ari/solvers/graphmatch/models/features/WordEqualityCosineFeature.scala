package org.allenai.ari.solvers.graphmatch.models.features

import org.allenai.ari.solvers.graphmatch.graph.path.{ EdgePath, PathTrait }
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import breeze.util.{ HashIndex, MutableIndex }
import breeze.linalg.SparseVector

import breeze.linalg.norm
import com.tinkerpop.blueprints.Vertex
import org.allenai.ari.solvers.graphmatch.tools.{ StopWord, PathToPhraseSeq }
import scala.util.Sorting

/** Created by TomK on 8/25/14.
  */
object WordEqualityCosineFeature extends LightweightLexicalFeatureTrait {

  weight = 3.0
  override val featureTag: String = "COSWORD"
  override val requireStemming = false

  override def wordsFeature(words1: Seq[String], words2: Seq[String], focus: Option[String]): Double = {

    if (words1.isEmpty || words2.isEmpty) return 0.0

    // Build vector representations of the two phrases.
    val vocabIndex = new HashIndex[String]()

    val w1 = words1.reduce(_ + " " + _)
    val w2 = words2.reduce(_ + " " + _)

    if (StopWord(w1) || StopWord(w2)) return 0.0

    // More loops than needed but these should be small loops.
    val indices1 = words1.map(w => vocabIndex.index(w.toLowerCase)).toArray
    val indices2 = words2.map(w => vocabIndex.index(w.toLowerCase)).toArray
    Sorting.quickSort(indices2) // SparseVector requires ordered indices.

    println(s"Indices1 : ${indices1}")
    println(s"Indices2 : ${indices2}")

    val vector1 = new SparseVector(Array.fill(indices1.size) { 1 }, indices1, vocabIndex.size)
    val vector2 = new SparseVector(Array.fill(indices2.size) { 1 }, indices2, vocabIndex.size)

    println(s"Vector1 : $vector1")
    println(s"Vector2 : $vector2")

    val cosSim: Double = (vector1 dot vector2) / (norm(vector1) * norm(vector2))

    cosSim

  }

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String] = None): Double = {

    qp match {
      case p: EdgePath => return 0.0
      case _ => {}
    }
    ep match {
      case p: EdgePath => return 0.0
      case _ => {}
    }

    val qw = PathToPhraseSeq(qp)
    val ew = PathToPhraseSeq(ep)

    wordsFeature(qw, ew)

  }

}
