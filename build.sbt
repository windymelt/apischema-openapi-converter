val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "apischema-openapi-converter",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.4.0",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
