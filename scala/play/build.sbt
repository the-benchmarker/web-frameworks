organization := "the-benchmarker"

name := "server"
scalaVersion := "2.13.12"

lazy val root = (project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      guice
    )
  )
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin))
