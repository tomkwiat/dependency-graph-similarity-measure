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

/** Created by TomK on 8/27/14.
  */
object StringCleaner {

  /** Copy of Dirk's version in common.
    *
    * Maps weird unicode characters to ASCII equivalents. This list comes from http://lexsrv3.nlm.nih.gov/LexSysGroup/Projects/lvg/current/docs/designDoc/UDF/unicode/DefaultTables/symbolTable.html
    */
  val unicodeCharMap = Map(
    '\u00AB' -> "\"",
    '\u00AD' -> "-",
    '\u00B4' -> "'",
    '\u00BB' -> "\"",
    '\u00F7' -> "/",
    '\u01C0' -> "|",
    '\u01C3' -> "!",
    '\u02B9' -> "'",
    '\u02BA' -> "\"",
    '\u02BC' -> "'",
    '\u02C4' -> "^",
    '\u02C6' -> "^",
    '\u02C8' -> "'",
    '\u02CB' -> "`",
    '\u02CD' -> "_",
    '\u02DC' -> "~",
    '\u0300' -> "`",
    '\u0301' -> "'",
    '\u0302' -> "^",
    '\u0303' -> "~",
    '\u030B' -> "\"",
    '\u030E' -> "\"",
    '\u0331' -> "_",
    '\u0332' -> "_",
    '\u0338' -> "/",
    '\u0589' -> ":",
    '\u05C0' -> "|",
    '\u05C3' -> ":",
    '\u066A' -> "%",
    '\u066D' -> "*",
    '\u200B' -> " ",
    '\u2010' -> "-",
    '\u2011' -> "-",
    '\u2012' -> "-",
    '\u2013' -> "-",
    '\u2014' -> "-",
    '\u2015' -> "--",
    '\u2016' -> "||",
    '\u2017' -> "_",
    '\u2018' -> "'",
    '\u2019' -> "'",
    '\u201A' -> ",",
    '\u201B' -> "'",
    '\u201C' -> "\"",
    '\u201D' -> "\"",
    '\u201E' -> "\"",
    '\u201F' -> "\"",
    '\u2032' -> "'",
    '\u2033' -> "\"",
    '\u2034' -> "''",
    '\u2035' -> "`",
    '\u2036' -> "\"",
    '\u2037' -> "''",
    '\u2038' -> "^",
    '\u2039' -> "<",
    '\u203A' -> ">",
    '\u203D' -> "?",
    '\u2044' -> "/",
    '\u204E' -> "*",
    '\u2052' -> "%",
    '\u2053' -> "~",
    '\u2060' -> " ",
    '\u20E5' -> "\\",
    '\u2212' -> "-",
    '\u2215' -> "/",
    '\u2216' -> "\\",
    '\u2217' -> "*",
    '\u2223' -> "|",
    '\u2236' -> ":",
    '\u223C' -> "~",
    '\u2264' -> "<=",
    '\u2265' -> ">=",
    '\u2266' -> "<=",
    '\u2267' -> ">=",
    '\u2303' -> "^",
    '\u2329' -> "<",
    '\u232A' -> ">",
    '\u266F' -> "#",
    '\u2731' -> "*",
    '\u2758' -> "|",
    '\u2762' -> "!",
    '\u27E6' -> "[",
    '\u27E8' -> "<",
    '\u27E9' -> ">",
    '\u2983' -> "{",
    '\u2984' -> "}",
    '\u3003' -> "\"",
    '\u3008' -> "<",
    '\u3009' -> ">",
    '\u301B' -> "]",
    '\u301C' -> "~",
    '\u301D' -> "\"",
    '\u301E' -> "\"",
    '\uFEFF' -> " ")

  def replaceWeirdUnicodeChars(string: String): String =
    unicodeCharMap.foldLeft(string) {
      case (s, (unicodeChar, replacement)) =>
        s.replace(unicodeChar.toString, replacement)
    }

}
