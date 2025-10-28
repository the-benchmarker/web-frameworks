val CaskVersion = "[0.11,0.12)"

name := "server"

scalaVersion := "3.7.3"

run / fork := true

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
	"com.lihaoyi" %% "cask" % CaskVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
