val ZioHttpVersion = "[3.0.0-RC1,3.1.0)"

name := "server"

scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
