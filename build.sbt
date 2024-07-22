import sbt.Keys.libraryDependencies

ThisBuild / version := "0.1.0"

ThisBuild / scalaVersion := "3.4.2"

val scalacticVersion           = "3.2.18"
val scalaTestVersion           = "3.2.18"
val zioVersion                 = "2.1.6"
val catsCoreVersion            = "2.12.0"
val disciplineCoreVersion      = "1.7.0"
val disciplineScalaTestVersion = "2.3.0"

lazy val root = (project in file("."))
  .settings(
    name             := "byronbay",
    idePackagePrefix := Some("my.playground"),
    libraryDependencies ++= Seq(
      // cats
      "org.typelevel" %% "cats-core"            % catsCoreVersion,
      "org.typelevel" %% "cats-laws"            % catsCoreVersion            % "test",
      "org.typelevel" %% "discipline-core"      % disciplineCoreVersion      % "test",
      "org.typelevel" %% "discipline-scalatest" % disciplineScalaTestVersion % "test",

      // zio
      "dev.zio" %% "zio" % zioVersion,

      // testing
      "org.scalactic" %% "scalactic" % scalacticVersion % "test",
      "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
    )
  )
