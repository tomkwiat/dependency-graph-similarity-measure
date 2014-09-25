package org.allenai.ari.solvers.graphmatch.tools

/** Created by TomK on 7/18/14.
  */
object StopWord {

  val stopWords: Set[String] = scala.io.Source.fromFile(
    "/Users/TomK/Dropbox/Projects/AllenAi/prototyping/dependencies/graphmatch/src/main/resources/StopWords.txt").getLines.map(w => w.toLowerCase()).toSet

  def apply(word: String): Boolean = {
    val lw = word.toLowerCase
    stopWords.contains(lw)
    //stopWords.contains(word)
  }

  //val a : Boolean = StopWord("asdf")

}
