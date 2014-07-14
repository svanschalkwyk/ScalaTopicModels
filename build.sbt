name := "topic-models"

version := "1.0"

scalaVersion := "2.11.1"

//libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.7" % "test"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.1.7" % "test",
  "edu.stanford.nlp" % "stanford-corenlp" % "1.3.4" artifacts (Artifact("stanford-corenlp", "models"), Artifact("stanford-corenlp")),
  "org.scalanlp" % "breeze_2.10" % "0.7",
  "org.scalanlp" % "breeze-natives_2.10" % "0.7"
)


