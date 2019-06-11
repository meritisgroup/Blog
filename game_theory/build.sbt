//Test / parallelExecution := false
Test / logBuffered := false

lazy val root = (project in file("."))
	.settings(
		name := "Meritis Blog - Game Theory",
		libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
	)
