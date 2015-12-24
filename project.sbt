name := "more"

description := "Thin language framework"

scalaVersion := "2.11.7"

///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val cacao = FDProject(
	"org.uqbar" %% "voodoo" % "1.3.5",
	"org.uqbar" %% "identity-map" % "latest.integration",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
	"org.scala-lang" % "scala-reflect" % "2.11.7",
	"org.scalatest" %% "scalatest" % "2.2.5" % "test",
	"org.uqbar" %% "parser-test" % "latest.integration" % "test"
)

///////////////////////////////////////////////////////////////////////////////////////////////////

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"