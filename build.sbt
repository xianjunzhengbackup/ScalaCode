name := "ScalaCode"
version := "0.1"
scalaVersion := "3.3.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test
)
Global / serverConnectionType := ConnectionType.Tcp // experimental
