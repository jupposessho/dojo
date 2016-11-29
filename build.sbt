name         := """dojo"""
version      := "1.0"
scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

val akkaV = "2.4.14"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaV
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % akkaV

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2"
)

fork in run := true
