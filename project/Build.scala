import sbt._
import Keys._

object BuildSettings {
  val buildOrganization = "com.sfsync"
  val buildName = "SFSync"
  val buildVersion = "0.2"
  val buildScalaVersion = "2.10.2"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "UTF-8"),
    resolvers := Seq(
      "JAnalyse Repository" at "http://www.janalyse.fr/repository/"
    ),
    autoScalaLibrary := true,
    offline := false)
}


object Dependencies {
  val scala = "org.scala-lang" % "scala-library" % BuildSettings.buildScalaVersion % "provided"
//  val scalaReflect = "org.scala-lang" % "scala-reflect" % BuildSettings.buildScalaVersion
  val akka = "com.typesafe.akka" %% "akka-actor" % "2.2.1"
  val scalafx = "org.scalafx" %% "scalafx-core" % "1.0-SNAPSHOT" // this is locally compiled
  val sftp = "fr.janalyse" %% "janalyse-ssh" % "0.9.10" % "compile"
  val h2 = "com.h2database" % "h2" % "1.3.173"
  val squeryl = "org.squeryl" %% "squeryl" % "0.9.5-6"


}

object WMPBuild extends Build {
  import Dependencies._
  import BuildSettings._

//  lazy val javaHome = {
//    var j = System.getenv("JAVAFX_HOME")
//    if (j == null) {
//      j = System.getenv("JAVA_HOME")
//      if (j == null) {
//        throw new RuntimeException(
//          "SBT Failure: neither JAVAFX_HOME nor JAVA_HOME environment variables have been defined!"
//        )
//      }
//    }
//    val dir = new File(j)
//    if (!dir.exists) {
//      throw new RuntimeException("SBT Failure: no such directory found: " + j)
//    }
//    println("**** detected Java/JDK Home is set to " + dir + "  ****")
//    Some(j)
//  }

  val javaHome = Some("/Library/Java/JavaVirtualMachines/jdk1.7.0_45.jdk/Contents/Home") // TODO use above if environm var read

  lazy val unmanagedListing = unmanagedJars in Compile += Attributed.blank(file(javaHome.get + "/jre/lib/jfxrt.jar"))

  lazy val sfsyncSettings = buildSettings ++ Seq(
    name := "SFSync",
    libraryDependencies ++= Seq(scala, akka, scalafx, sftp, h2, squeryl),
    unmanagedListing
  )

  lazy val root = Project(
    id = "sfsync",
    base = file("."),
    settings = sfsyncSettings
  ).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)

}
