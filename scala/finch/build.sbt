val FinagleVersionRange = "[0.32,0.33)"

name := "server"
scalaVersion := "2.13.5"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finchx-core" % FinagleVersionRange
    )
  ).enablePlugins(JavaAppPackaging)