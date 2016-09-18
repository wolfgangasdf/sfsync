import sbt._
import sbt.Keys._
import sbtbuildinfo._

import java.time.ZonedDateTime

object Build extends Build {
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
      scalaVersion := "2.11.8",
      scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-encoding", "UTF-8"),
      libraryDependencies ++= Seq(
        "org.scalafx" %% "scalafx" % "8.0.92-R10" withSources() withJavadoc(),
        "com.hierynomus" % "sshj" % "0.17.2" withSources() withJavadoc(),
        "org.slf4j" % "slf4j-simple" % "1.7.21"
      )
    )
  ).enablePlugins(BuildInfoPlugin).settings(
    BuildInfoKeys.buildInfoKeys := Seq[BuildInfoKey](
      name, version, scalaVersion, sbtVersion,
      BuildInfoKey.action("buildTime") { ZonedDateTime.now.toString } // re-computed each time at compile
    ),
    BuildInfoKeys.buildInfoPackage := "sfsync",
    BuildInfoKeys.buildInfoUsePackageAsPath := true
  )
}

