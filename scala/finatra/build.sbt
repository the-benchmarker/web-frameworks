name := "server"
scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[24.2,24.3)"
    )
  )
  .enablePlugins(JavaAppPackaging)
