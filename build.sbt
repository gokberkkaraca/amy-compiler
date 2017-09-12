name := "amyc"

version := "1.0"

scalaVersion := "2.12.3"

scalaSource in Compile := baseDirectory.value / "src"

mainClass in (Compile, run) := Some("amyc.Main")

mainClass in (Compile, packageBin) := Some("amyc.Main")

scalaSource in Test := baseDirectory.value / "test" / "scala"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v")

