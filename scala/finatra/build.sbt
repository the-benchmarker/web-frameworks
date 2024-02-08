name := "server"
scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[23.11,23.12)"
    )
  )
  .enablePlugins(JavaAppPackaging)
