val ZioHttpVersion = "3.11.4"

name := "server"

scalaVersion := "3.8.4"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
