organization := "the-benchmarker"

name := "server"
scalaVersion := "2.13.12"

lazy val root = (project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      guice,
      "com.typesafe.play" %% "play-json" % "[2.9,2.10)"
    )
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin))
