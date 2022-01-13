name := "server"
scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[21.12,21.13)"
    )
  ).enablePlugins(JavaAppPackaging)
