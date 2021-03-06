name := "tic-tac-toe"
organization := "com.carvana"
version := "0.1"
scalaVersion := "2.13.4"
//logLevel := Level.Debug
// Having problems interrupting test runs with Ctrl+C
cancelable in Global := true

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.1.3" % Runtime,
  "org.rogach" %% "scallop" % "4.0.2",
)
