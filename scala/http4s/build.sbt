val Http4sVersionRange = "[0.23,0.24)"

name := "server"
scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % Http4sVersionRange,
      "org.http4s" %% "http4s-dsl" % Http4sVersionRange
    )
  )
  .enablePlugins(JavaAppPackaging)
