val Http4sVersionRange = "[0.21,0.22)"

name := "server"
scalaVersion := "2.13.5"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % Http4sVersionRange,
      "org.http4s" %% "http4s-dsl" % Http4sVersionRange
    )
  ).enablePlugins(JavaAppPackaging)
