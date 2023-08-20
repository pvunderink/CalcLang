ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "CalcLang"
  )
