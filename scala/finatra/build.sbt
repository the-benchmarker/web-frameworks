name := "server"
scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[22.2,22.3)"
    )
  ).enablePlugins(JavaAppPackaging)
