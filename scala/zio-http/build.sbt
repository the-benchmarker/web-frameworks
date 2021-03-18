name := "server"
scalaVersion := "2.13.5"

lazy val zhttp = ProjectRef(uri(s"git://github.com/dream11/zio-http.git"), "zhttp")
lazy val `zio-http` = (project in file(".")).dependsOn(zhttp)

enablePlugins(JavaAppPackaging)
