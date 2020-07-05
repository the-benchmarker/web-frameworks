organization := "the-benchmarker"

name := "server"
scalaVersion := "2.13.2"

lazy val root = (project.in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      guice,
      "com.typesafe.play" %% "play-json" % "[2.7,2.8)"
    )
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin))
