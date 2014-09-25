package org.allenai.ari.solvers.graphmatch.models.features

import org.allenai.ari.solvers.graphmatch.graph.path.{ MultiNodePath, SingleNodePath, PathTrait }
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import breeze.linalg._
import org.allenai.ari.solvers.graphmatch.graph.path.SingleNodePath
import org.allenai.ari.solvers.graphmatch.graph.path.MultiNodePath
import breeze.util.Index

/** Created by TomK on 8/4/14.
  */
object VsmTfidfFeature extends LightweightLexicalFeatureTrait {

  override val requireStemming = false

  val tfidfFile = ""

  //  val tfidfCache : Map[String,Double] = scala.io.Source.fromFile(tfidfFile).asJsObject().fields.map(kv => kv._1.replace("\"","") -> kv._2.toString.toDouble)
  val tfidfCache: Map[String, Double] = Map.empty
  val wordIndex = Index[String]() // This doesn't need to be initialized

  override def wordsFeature(words1: Seq[String], words2: Seq[String], focus: Option[String]): Double = {
    Double.NegativeInfinity
  }

  def bowFeature(w1: Seq[String], w2: Seq[String]): Double = {

    val vb1 = new VectorBuilder[Double](Int.MaxValue)
    for (w <- w1) {
      vb1.add(wordIndex(w), tfidfCache.getOrElse(w, 0.0)) // What about the stem?
    }

    val vb2 = new VectorBuilder[Double](Int.MaxValue)
    for (w <- w2) {
      vb2.add(wordIndex(w), tfidfCache.getOrElse(w, 0.0)) // What about the stem?
    }

    val v1 = vb1.toSparseVector
    val v2 = vb2.toSparseVector

    v1.dot(v2) / (norm(v1) * norm(v2))

  }

  override def feature(qp: PathTrait, ep: PathTrait, graph: TinkerGraph, focus: Option[String]): Double = {

    val qw = qp match {
      case p: SingleNodePath => Seq(p.inNode.getProperty[String]("text"))
      case p: MultiNodePath => p.containedNodes.map(n => n.getProperty[String]("text"))
      case _ => return 0.0
    }
    val ew = ep match {
      case p: SingleNodePath => Seq(p.inNode.getProperty[String]("text"))
      case p: MultiNodePath => p.containedNodes.map(n => n.getProperty[String]("text"))
      case _ => return 0.0
    }

    bowFeature(qw, ew)

  }

  def generateTfIdf(corpusFile: String) {
    val sentences = None
  }

  override val featureTag: String = "VSMTfidf"
}
