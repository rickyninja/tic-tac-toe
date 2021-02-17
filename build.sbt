name := "tic-tac-toe"
organization := "com.carvana"
version := "0.1"
scalaVersion := "2.13.4"
//logLevel := Level.Debug
// Having problems interrupting test runs with Ctrl+C
cancelable in Global := true

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.2" % "test"
)
