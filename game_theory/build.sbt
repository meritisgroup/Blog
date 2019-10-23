//Test / parallelExecution := false
Test / logBuffered := false

resolvers += "Sonatype OSS Snapshots" at
	"https://oss.sonatype.org/content/repositories/snapshots"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

lazy val root = (project in file("."))
	.settings(
		name := "Meritis Blog - Game Theory",
		libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
		libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.18"
	)
