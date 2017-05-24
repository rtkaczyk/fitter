name := "fitter"

version := "1.0"

scalaVersion := "2.12.2"

mainClass in assembly := Some("eu.rtkaczyk.fitter.Fitter")
//assemblyJarName in assembly := "fitter.jar"
//target in assembly := file(".")
assemblyOutputPath in assembly := file("target/fitter.jar")
