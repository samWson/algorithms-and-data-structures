name := "Agorithms and Data Structures"

ThisBuild / scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.1.1",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test"
)

scalacOptions := Seq("-feature", "-deprecation")
