val ZioHttpVersion = "3.0.0-RC4"

name := "server"

scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
