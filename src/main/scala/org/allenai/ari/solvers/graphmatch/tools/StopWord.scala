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

import scala.io.Source

/** Created by TomK on 7/18/14.
  */
object StopWord {

  val stopWords: Set[String] = Source.fromURL(getClass.getResource("/StopWords.txt")).getLines.map(w => w.toLowerCase()).toSet

  def apply(word: String): Boolean = {
    val lw = word.toLowerCase
    stopWords.contains(lw)
    //stopWords.contains(word)
  }

  //val a : Boolean = StopWord("asdf")

}
