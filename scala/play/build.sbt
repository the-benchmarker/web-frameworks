organization := "the-benchmarker"

name := "server"
scalaVersion := "2.13.3"

lazy val root = (project.in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      guice,
      "com.typesafe.play" %% "play-json" % "[2.8,2.9)"
    )
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin))
