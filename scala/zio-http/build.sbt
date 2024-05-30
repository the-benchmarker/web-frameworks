val ZioHttpVersion = "3.0.0-RC8"

name := "server"

scalaVersion := "3.4.2"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
