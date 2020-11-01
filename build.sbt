name := "course_algorithms_fall_2020"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies += "org.vegas-viz" %% "vegas" % "0.3.11"
libraryDependencies += "com.github.bruneli.scalaopt" % "scalaopt-core_2.11" % "0.2"
libraryDependencies += "org.scala-lang.modules" % "scala-java8-compat_2.11" % "0.9.1"

libraryDependencies ++=  Seq(
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2",
  "org.scalanlp" %% "breeze-viz" % "0.11.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
)


