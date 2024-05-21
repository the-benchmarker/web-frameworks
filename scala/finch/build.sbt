val FinagleVersionRange = "[24.5,24.6)"

name := "server"
scalaVersion := "2.13.14"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finch-core" % FinagleVersionRange
    )
  )
  .enablePlugins(JavaAppPackaging)
