organization := "the-benchmarker"

name := "server"
scalaVersion := "3.3.1"

lazy val root = (project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      guice
    )
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin))
