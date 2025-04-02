val ZioHttpVersion = "3.2.0"

name := "server"

scalaVersion := "3.6.4"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
