val ZioHttpVersion = "3.11.6"

name := "server"

scalaVersion := "3.9.0"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
