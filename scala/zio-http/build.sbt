val ZioHttpVersion = "3.10.0"

name := "server"

scalaVersion := "3.8.2"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
