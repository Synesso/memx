import sbt._
import sbt.Keys._

object ProjectBuild extends Build {

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Mem_X",
      organization := "net.badgerhunt",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.1",
      resolvers ++= Seq(
        "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
        "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
        "releases"  at "http://oss.sonatype.org/content/repositories/releases"),
      libraryDependencies ++= Seq(
        "com.typesafe.akka" % "akka-actor" % "2.0",
        "org.specs2" %% "specs2" % "1.9" % "test")
    )
  )
}
