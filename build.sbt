val sv = "2.13.8"

lazy val root = project
  .in(file("."))
  .settings(
    name := "apischema-openapi-converter",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := sv,
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.4.0",
      "org.scalactic" %% "scalactic" % "3.2.13",
      "org.scalatest" %% "scalatest" % "3.2.13" % Test,
    )
  )
