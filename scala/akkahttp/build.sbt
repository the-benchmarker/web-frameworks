name := "server"
scalaVersion := "3.4.2"

resolvers += "Akka library repository".at("https://repo.akka.io/maven")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "[10.6,10.7)"
)

enablePlugins(JavaAppPackaging)
