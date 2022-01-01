name := "server"
scalaVersion := "2.13.6"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[21.12,21.13)"
    )
  ).enablePlugins(JavaAppPackaging)
