name := "server"
scalaVersion := "2.13.4"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finchx-core" % "0.32.1"
    )
  ).enablePlugins(JavaAppPackaging)