ThisBuild / version := "1.0" 
ThisBuild / scalaVersion := "2.12.11" 

lazy val fpnscala = (project in file("."))
    .settings(
        name := "fp-n-scala"
	)
