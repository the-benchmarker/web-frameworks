val Http4sVersion = "0.18.17"

lazy val root = (project in file("."))
  .settings(
    organization := "the.benchmarker",
    name := "http4s",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.12.6",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion
    )
  )

