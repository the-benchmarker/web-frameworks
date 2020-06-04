name := "web-frameworks-play"
organization := "the-benchmarker"

scalaVersion := "2.13.2"

lazy val root = (project.in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      guice,
      "com.typesafe.play" %% "play-json" % "[2.8,2.9)"
    )
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin))