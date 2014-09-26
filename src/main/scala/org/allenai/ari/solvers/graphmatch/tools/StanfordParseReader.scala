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
import com.tinkerpop.blueprints.Vertex
import scala.collection.mutable
import edu.knowitall.tool.stem.MorphaStemmer

/*
* Read in Stanford Parses in dep(parent,child) format.
*/
object StanfordParseReader {

  val stemmer = new MorphaStemmer()

  val dependenciesToIgnore = Seq() // Can choose to ignore certain dependencies. Not currently used.

  // val depMatcher = new scala.util.matching.Regex("""(.*)\((.*), (.*)\)""", "depType", "head", "child")

  // Ignore self loops of the form 1->1'.
  val selfLoopedDepMatcher = new scala.util.matching.Regex("""\d'""")

  def readParse(dependencies: Iterator[String]): TinkerGraph = {

    val graph = new TinkerGraph()
    val nodes = new mutable.HashMap[String, Vertex]

    for (dep <- dependencies) {

      if (!dep.startsWith("#") && !selfLoopedDepMatcher.findAllMatchIn(dep).hasNext) {

        val depType = dep.split("""\(""")(0).replace(")", "").replace("(", "")
        val head = dep.split("""\(""")(1).split(", ")(0).replace(")", "").replace("(", "")
        val child = dep.split("""\(""")(1).split(", ")(1).replace(")", "").replace("(", "")

        val headNode: Vertex = nodes.get(head) match {
          case None => {
            val n = graph.addVertex(head)
            n.setProperty("text", head.split("-")(0))
            n.setProperty("stemmedtext", stemmer.stem(n.getProperty("text")))
            n.setProperty("position", head.split("-").last.toInt)
            nodes.put(head, n)
            n
          }
          case Some(n) => n
        }

        val childNode: Vertex = nodes.get(child) match {
          case None => {
            val n = graph.addVertex(child)
            n.setProperty("text", child.split("-")(0))
            n.setProperty("stemmedtext", stemmer.stem(n.getProperty("text")))
            n.setProperty("position", child.split("-").last.toInt)
            nodes.put(child, n)
            n
          }
          case Some(n) => n
        }

        graph.addEdge(null, headNode, childNode, depType)
      }

    }
    graph
  }

  def readFile(filename: String) {
    val dependencies = scala.io.Source.fromFile(filename)
    readParse(dependencies.getLines())
  }

  def readCorpus(corpus: Iterator[String]): Seq[TinkerGraph] = {

    var parses = mutable.Seq[TinkerGraph]()
    var parseCounter = 0
    while (corpus.hasNext) {
      val sentence = corpus.next
      if (sentence != "" && sentence.contains("ROOT-0")) {
        parses = parses.+:(readParse(sentence.split("###").iterator))
        parseCounter += 1
      }
    }
    println(s"Got ${parseCounter} parses")
    parses
  }

}
