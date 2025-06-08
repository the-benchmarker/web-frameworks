val ZioHttpVersion = "3.3.3"

name := "server"

scalaVersion := "3.6.4"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
