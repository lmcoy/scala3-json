val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % "test",
      "org.scalatest" %% "scalatest-flatspec" % "3.2.9" % "test"
    )
  )
