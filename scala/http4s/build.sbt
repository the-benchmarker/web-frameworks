val Http4sVersionRange = "[0.23,0.24)"

name := "server"
scalaVersion := "3.7.4"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % Http4sVersionRange,
      "org.http4s" %% "http4s-dsl" % Http4sVersionRange
    )
  )
  .enablePlugins(JavaAppPackaging)
