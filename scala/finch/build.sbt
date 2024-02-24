val FinagleVersionRange = "[0.34,0.35)"

name := "server"
scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finch-core" % FinagleVersionRange cross CrossVersion.for3Use2_13
    )
  )
  .enablePlugins(JavaAppPackaging)
