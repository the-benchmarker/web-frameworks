val Http4sVersionRange = "[1.0,2.0)"

name := "server"
scalaVersion := "2.13.4"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % Http4sVersionRange,
      "org.http4s" %% "http4s-circe" % Http4sVersionRange,
      "org.http4s" %% "http4s-dsl" % Http4sVersionRange
    )
  ).enablePlugins(JavaAppPackaging)
