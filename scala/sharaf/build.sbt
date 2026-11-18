ThisBuild / scalaVersion := "3.10.0"

lazy val root = (project in file("."))
  .settings(
    name := "server",
    libraryDependencies += "ba.sake" %% "sharaf-potateh" % "0.23.0",
    Compile / mainClass := Some("Main")
  )
  .enablePlugins(JavaAppPackaging)
