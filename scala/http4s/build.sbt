val Http4sVersionRange = "[0.21,0.22)"

organization := "the.benchmarker"
name := "http4s"
version := "0.0.2"
scalaVersion := "2.13.1"
maintainer := "pippo"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % Http4sVersionRange,
      "org.http4s" %% "http4s-circe" % Http4sVersionRange,
      "org.http4s" %% "http4s-dsl" % Http4sVersionRange
    )
  ).enablePlugins(JavaAppPackaging)
