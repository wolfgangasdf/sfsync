import sbt._
import sbt.Keys._
import sbtbuildinfo.Plugin._

//import no.vedaadata.sbtjavafx.JavaFXPlugin
//import no.vedaadata.sbtjavafx.JavaFXPlugin.JFX

object Build extends Build {
  lazy val myBuildInfoSettings = Seq(
    sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
    buildInfoPackage := "sfsync"
  )
  lazy val sfsync = Project(
    id = "sfsync",
    base = file("."),
    /* should work but doesn't (sbt run can't find main class), use build.sbt for now:
      settings = Defaults.coreDefaultSettings ++ JavaFXPlugin.jfxSettings ++ Seq(
        JFX.mainClass := Some("sgar.Sgar"),
     */
    settings = Defaults.coreDefaultSettings ++ Seq(
      name := "SFSync",
      organization := "sfsync",
      version := "0.9-SNAPSHOT",
      scalaVersion := "2.11.7",
      scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-encoding", "UTF-8"),
      libraryDependencies ++= Seq(
        "org.scalafx" %% "scalafx" % "8.0.60-R9",
        "com.typesafe.akka" %% "akka-actor" % "2.4.0",
        "com.jcraft" % "jsch" % "0.1.53"
      )
    ) ++ buildInfoSettings ++ myBuildInfoSettings
  )
}

