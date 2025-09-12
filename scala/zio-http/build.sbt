val ZioHttpVersion = "3.5.1"

name := "server"

scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
