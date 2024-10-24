name := "ScalaCode"
version := "0.1"
scalaVersion := "3.3.3"

libraryDependencies += "org.creativescala" %% "doodle-core" % "0.26.0" from "https://repo1.maven.org/maven2/org/creativescala/doodle-core_3/0.26.0/doodle-core_3-0.26.0.jar"
//libraryDependencies += "org.creativescala" %% "doodle" % "0.26.0" from "https://repo1.maven.org/maven2/org/creativescala/doodle_3/0.26.0/doodle_3-0.26.0.jar"
libraryDependencies += "org.creativescala" %% "doodle-image" % "0.26.0" from "https://repo1.maven.org/maven2/org/creativescala/doodle-image_3/0.26.0/doodle-image_3-0.26.0.jar"
libraryDependencies += "org.creativescala" %% "doodle-java2d" % "0.26.0" from "https://repo1.maven.org/maven2/org/creativescala/doodle-java2d_3/0.26.0/doodle-java2d_3-0.26.0.jar"
libraryDependencies += "org.scalatest" %% "scalatest-core" % "3.3.0-SNAP4" from "https://repo1.maven.org/maven2/org/scalatest/scalatest-core_3/3.3.0-SNAP4/scalatest-core_3-3.3.0-SNAP4.jar"
libraryDependencies += "org.scalatest" %% "scalatest-shouldmatchers" % "3.3.0-SNAP4" from "https://repo1.maven.org/maven2/org/scalatest/scalatest-shouldmatchers_3/3.3.0-SNAP4/scalatest-shouldmatchers_3-3.3.0-SNAP4.jar"
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.3.0-SNAP4" from "https://repo1.maven.org/maven2/org/scalatest/scalatest-flatspec_3/3.3.0-SNAP4/scalatest-flatspec_3-3.3.0-SNAP4.jar"

Global / serverConnectionType := ConnectionType.Tcp // experimental
