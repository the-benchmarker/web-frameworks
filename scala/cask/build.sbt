val CaskVersion = "0.10.1"

name := "server"

scalaVersion := "3.5.2"

run / fork := true

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
	"com.lihaoyi" %% "cask" % CaskVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
