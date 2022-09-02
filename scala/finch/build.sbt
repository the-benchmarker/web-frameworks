val FinagleVersionRange = "[0.33,0.34)"

name := "server"
scalaVersion := "3.2.+"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finchx-core" % FinagleVersionRange
    )
  ).enablePlugins(JavaAppPackaging)