val ZioHttpVersion = "3.0.0"

name := "server"

scalaVersion := "3.5.1"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
