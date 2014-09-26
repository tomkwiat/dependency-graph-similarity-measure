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


package org.allenai.ari.solvers.graphmatch

import org.allenai.ari.solvers.graphmatch.models.{DeletionModel, Model}
import org.allenai.ari.solvers.graphmatch.models.features.{FocusMatchFeature, TfIdfCosineFeature}
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import org.allenai.ari.solvers.graphmatch.tools.{GraphToPhrase, GraphToStemmedWords}
import scala.collection.parallel.mutable.ParArray
import org.allenai.ari.solvers.graphmatch.graph.path.{PathPair, SentencePaths}
import scala.collection.mutable

/**
 * Created by TomK on 9/25/14.
 */
class Solver {

  def name = this.getClass.getSimpleName

  val maxLengthConsidered = 200

  val matchModel = Model(Seq(TfIdfCosineFeature, FocusMatchFeature))
  // MultiEdgePathWordDeletionFeature, NodePathDeletionFeature))

  val deletionModel = DeletionModel(Seq.empty)
  // Seq(NodeDeletionFeature))

  def solve(questionStatementParse : TinkerGraph,
            answerString : String,
            evidencePaths: ParArray[(SentencePaths, TinkerGraph)]) : Double = {

    println(s"Question Statement : ${GraphToPhrase(questionStatementParse)}")
    scoreParsedQuestionStatement(questionStatementParse, answerString, evidencePaths)

  }

  /* The bag of words score is cheap to compute and is used to decide whether or
    not we want to pass a given sentence to the more expensive graph matching solver. */

  def getBowScore(questionStatement: TinkerGraph, evidenceStatement: TinkerGraph, model: Model, focus: String): Double = {

    val qw: Seq[String] = GraphToStemmedWords(questionStatement.getVertex("ROOT-0"))
    val ew: Seq[String] = GraphToStemmedWords(evidenceStatement.getVertex("ROOT-0"))

    val lightWeightLexScore = model.lightWeightLexScore(qw, ew, Some(focus))
    lightWeightLexScore

  }

  def scoreParsedQuestionStatement(questionStatement: TinkerGraph, focus : String,
                                   evidencePaths: ParArray[(SentencePaths,TinkerGraph)]): Double = {

    // These things could probably go in a configuration file.
    var score = 0.0
    val bowThreshold = 0.1
    val numberOfSentencesToConsider = 3

    val matchQueue =
      new mutable.SynchronizedPriorityQueue[(PathPair, TinkerGraph, String, Double)]()(
        Ordering.by[(PathPair, TinkerGraph, String, Double), Double](_._4))

    val questionSentencePaths = SentencePaths(questionStatement.getVertex("ROOT-0"), questionStatement)
    questionSentencePaths.addPaths(questionStatement.getVertex("ROOT-0"))

    evidencePaths.par.foreach(epg => {
      val bowScore: Double = getBowScore(questionStatement, epg._2, matchModel, focus)
      if (bowScore > bowThreshold) {

        val gm = GraphMatcher(questionSentencePaths, questionStatement, focus, epg._1, epg._2, matchModel, deletionModel)

        gm.initializeAllPathPathMatches
        gm.matchVertices(questionStatement.getVertex("ROOT-0"), epg._2.getVertex("ROOT-0"))

        val m = gm.headHeadIndexedMaxPathPairs((questionStatement.getVertex("ROOT-0"), epg._2.getVertex("ROOT-0")))

        matchQueue.+=((m._1, epg._2, gm.bestPathVisualizable, m._2))
      } else {}
    })

    for (i <- 1 to math.min(numberOfSentencesToConsider, matchQueue.size)) {
      val m = matchQueue.dequeue
      score += m._4
    }

    println(s"Score for ${GraphToPhrase(questionStatement)} is $score")

    score

  }





}
