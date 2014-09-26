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

import org.allenai.ari.solvers.graphmatch.tools.StanfordParseReader
import org.allenai.ari.solvers.graphmatch.graph.path.SentencePaths
import scala.collection.parallel.mutable.ParArray

/**
 * Created by TomK on 9/25/14.
 */
object SolverExample extends App {

  // Plants reproduce by forming seeds from the male (pollen) and female (eggs)
  // cells found in the flower.
   val es1 = StanfordParseReader.readParse(
    ("nsubj(reproduce-2, Plants-1)###root(ROOT-0, reproduce-2)" +
    "###prepc_by(reproduce-2, forming-4)###dobj(forming-4, seeds-5)" +
    "###det(pollen-10, the-7)###amod(pollen-10, male-8)" +
    "###prep_from(forming-4, pollen-10)###amod(cells-17, female-13)" +
    "###appos(cells-17, eggs-15)###nsubj(found-18, cells-17)" +
    "###conj_and(reproduce-2, found-18)###det(flower-21, the-20)" +
    "###prep_in(found-18, flower-21)").split("###").iterator)
    val ep1 = new SentencePaths(es1.getVertex("ROOT-0"), es1)
    ep1.addPaths(es1.getVertex("ROOT-0"))

  // The part of the plant that is responsible for the production of seeds.
  val es2 = StanfordParseReader.readParse(
    ("det(flower-2, The-1)###nsubj(part-5, flower-2)###cop(part-5, is-3)" +
      "###det(part-5, the-4)###root(ROOT-0, part-5)###det(plant-8, the-7)" +
      "###prep_of(part-5, plant-8)###amod(plant-8, responsible-9)" +
      "###det(production-12, the-11)###prep_for(responsible-9, production-12)" +
      "###prep_of(production-12, seeds-14)").split("###").iterator)
  val ep2 = new SentencePaths(es2.getVertex("ROOT-0"), es2)
  ep2.addPaths(es2.getVertex("ROOT-0"))

  // A flower produces the seeds.
  val qs = StanfordParseReader.readParse(
    ("det(flower-2, A-1)###nsubj(produces-3, flower-2)" +
      "###root(ROOT-0, produces-3)###det(seeds-5, the-4)" +
      "###dobj(produces-3, seeds-5)").split("###").iterator)
  val answerString = "flower"

  val evidenceStatements = ParArray((ep1,es1), (ep2,es2))

  val solver = new Solver

  solver.solve(qs, answerString, evidenceStatements)


}
