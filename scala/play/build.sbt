organization := "the-benchmarker"

name := "server"
scalaVersion := "3.3.1"

lazy val root = (project
  .in(file("."))
  .settings(
    PlayKeys.akkaHttpScala3Artifacts := true,
    libraryDependencies ++= Seq(
      guice,
      "com.typesafe.play" %% "play-json" % "[2.10,2.11)"
    )
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin))
