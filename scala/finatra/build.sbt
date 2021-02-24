name := "server"
scalaVersion := "2.13.5"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.twitter" %% "finatra-http" % "[21.1,21.2)"
    )
  ).enablePlugins(JavaAppPackaging)