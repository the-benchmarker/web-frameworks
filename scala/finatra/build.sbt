name := "server"
scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[22.12,22.13)"
    )
  )
  .enablePlugins(JavaAppPackaging)
