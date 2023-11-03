lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .settings(
    name := "server",
    organization := "com.example",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.3.1",
    libraryDependencies ++= Seq(
      guice
    )
  )
