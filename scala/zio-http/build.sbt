val ZioHttpVersion = "3.0.1"

name := "server"

scalaVersion := "3.5.0"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
