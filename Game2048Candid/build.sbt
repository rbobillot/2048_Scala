name := "Game2048Candid"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation"
)

libraryDependencies ++= Seq(
  "com.1stleg" % "jnativehook" % "2.0.3"
)

assemblyJarName in assembly := "game2048_candid.jar"