name := "functadelic"

scalaVersion := "2.11.2"

scalaOrganization := "org.scala-lang.virtualized"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scala-lang.lms" %% "lms-core" % "1.0.0-SNAPSHOT",
  "org.scala-lang.plugins" % "scala-continuations-library_2.11" % "1.0.2",
  "com.github.manojo" % "lms-utils_2.11" % "0.1-SNAPSHOT",
  "com.github.manojo" % "lms-testutils_2.11" % "0.1-SNAPSHOT" % "test"
)

scalacOptions ++= Seq(
  "-Yvirtualize",
  "-P:continuations:enable",
  //"-optimize",
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions"
  //"-Yinline-warnings"
)

/**
 * tests are not thread safe
 * this applies to all lms tests that write
 * to a file, and do diff tests
 */
parallelExecution in Test := false

autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2")

defaultScalariformSettings
