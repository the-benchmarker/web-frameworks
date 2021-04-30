name := "server"
scalaVersion := "2.13.5"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http-server" % "[21.4,21.5)"
    )
  ).enablePlugins(JavaAppPackaging)
