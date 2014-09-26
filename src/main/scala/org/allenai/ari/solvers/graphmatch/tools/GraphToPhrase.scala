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


package org.allenai.ari.solvers.graphmatch.tools

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.{ Direction, Vertex }

import scala.collection.JavaConversions._
import org.allenai.ari.solvers.graphmatch.graph.path.{ PathTrait, MultiNodePath, SingleNodePath }

/** Created by TomK on 7/14/14.
  */

object GraphToPhrase {
  def apply(g: TinkerGraph): String = NodesToPhrase(g.getVertices.toSeq)
}

object NodesToPhrase {
  def apply(nodes: Seq[Vertex]): String = {
    if (nodes.isEmpty) {
      ""
    } else {
      NodesToPhraseSeq(nodes).reduce(_ + " " + _)
    }
  }
}

object NodesToPhraseSeq {
  def apply(nodes: Seq[Vertex]): Seq[String] = {
    nodes.map(n => (n.getProperty("text"): String, n.getProperty("position"): Int)).sortBy[Int](_._2).map(p => p._1)
  }
}

object PathToPhraseSeq {
  def apply(path: PathTrait): Seq[String] = {
    path match {
      case path: SingleNodePath => Seq(path.outNode.getProperty("text").toString)
      case path: MultiNodePath => NodesToPhraseSeq(path.containedNodes)
      case _ => Seq.empty
    }
  }

  object PathToStemmedPhraseSeq {
    def apply(path: PathTrait): Seq[String] = {
      path match {
        case path: SingleNodePath => Seq(path.outNode.getProperty("stemmedtext").toString)
        case path: MultiNodePath => NodesToPhraseSeq(path.containedNodes)
        case _ => Seq.empty
      }
    }
  }
}

object RootToPhrase {

  def apply(root: Vertex): String = {
    getLabels(root).foldLeft("")((s, l) => s + " " + l._1).toString
  }

  def getLabels(root: Vertex): Seq[(String, Int)] = {
    val word = root.getProperty("text").toString
    val position: Int = root.getProperty("position")
    Seq((word, position)).++(root.getVertices(Direction.OUT).flatMap(v => getLabels(v))).sortBy(e => e._2)
  }
}

object GraphToWords {

  def apply(root: Vertex): Seq[String] = {
    root.getVertices(Direction.OUT).flatMap(v => getLabels(v)).toSeq
  }

  def getLabels(root: Vertex): Seq[String] = {
    val word = root.getProperty("text").toString
    Seq(word).++(root.getVertices(Direction.OUT).flatMap(v => getLabels(v)))
  }
}

object GraphToStemmedWords {

  def apply(root: Vertex): Seq[String] = {
    root.getVertices(Direction.OUT).flatMap(v => getLabels(v)).toSeq
  }

  def getLabels(root: Vertex): Seq[String] = {
    val word = root.getProperty("stemmedtext").toString
    Seq(word).++(root.getVertices(Direction.OUT).flatMap(v => getLabels(v)))
  }
}