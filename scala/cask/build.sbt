val CaskVersion = "[0.10,0.11)"

name := "server"

scalaVersion := "3.6.3"

run / fork := true

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
	"com.lihaoyi" %% "cask" % CaskVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
