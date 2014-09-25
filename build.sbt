
name := "solvers-graphmatch"

description := "Graph matching solver implementation"

version := "0.0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  // Breeze stuff
  "org.scalanlp" %% "breeze" % "0.8.1",
  // Graph stuff
  "com.michaelpollmeier" %% "gremlin-scala" % "2.5.2",
  // Stemmer stuff
  "edu.washington.cs.knowitall.nlptools" %% "nlptools-stem-morpha" % "2.4.4"
)
