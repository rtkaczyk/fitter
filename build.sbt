name := "fitter"

version := "1.0"

scalaVersion := "2.12.2"

libraryDependencies += "com.google.guava" % "guava" % "22.0"

mainClass in assembly := Some("eu.rtkaczyk.fitter.Fitter")
assemblyOutputPath in assembly := file("target/fitter.jar")
