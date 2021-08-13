name := "server"
scalaVersion := "2.13.6"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[21.8,21.9)"
    )
  ).enablePlugins(JavaAppPackaging)
