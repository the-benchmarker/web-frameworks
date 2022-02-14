name := "server"
scalaVersion := "3.1.1"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[22.1,22.2)"
    )
  ).enablePlugins(JavaAppPackaging)
