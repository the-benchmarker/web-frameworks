val FinagleVersionRange = "[0.34,0.35)"

name := "server"
scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finch-core" % FinagleVersionRange
    )
  )
  .enablePlugins(JavaAppPackaging)
