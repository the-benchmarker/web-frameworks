name := "server"
scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[23.11,23.12)" cross CrossVersion.for3Use2_13
    )
  )
  .enablePlugins(JavaAppPackaging)
