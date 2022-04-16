ThisBuild / version := "1.0" 
ThisBuild / scalaVersion := "2.13.8" 

lazy val fpnscala = (project in file("."))
    .settings(
        name := "fp-n-scala"
	)
