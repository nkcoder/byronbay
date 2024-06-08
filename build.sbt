ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.4.2"

val scalacticVersion = "3.2.18"
val scalatestVersion = "3.2.18"

lazy val root = (project in file("."))
  .settings(
    name                                   := "byronbay",
    idePackagePrefix                       := Some("my.playground"),
    libraryDependencies += "org.scalactic" %% "scalactic" % scalacticVersion % "test",
    libraryDependencies += "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  )
