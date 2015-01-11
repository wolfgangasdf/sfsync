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
      scalaVersion := "2.11.4",
      scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-encoding", "UTF-8"),
      libraryDependencies ++= Seq(
        "org.scalafx" %% "scalafx" % "8.0.20-R6",
        "com.typesafe.akka" %% "akka-actor" % "2.3.7",
        "com.jcraft" % "jsch" % "0.1.51"
      )
    ) ++ buildInfoSettings ++ myBuildInfoSettings
  )
}

/*

object BuildSettings {
  val buildOrganization = "com.wolle.sfsync"
  val buildName = "SFSync"
  val buildVersion = "0.2"
  val buildScalaVersion = "2.11.4"

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "UTF-8"),
//    resolvers := Seq(
//      "JAnalyse Repository" at "http://www.janalyse.fr/repository/"
//    ),
    autoScalaLibrary := true,
    offline := false)
}


object Dependencies {
  val scala = "org.scala-lang" % "scala-library" % BuildSettings.buildScalaVersion % "provided"
//  val scalaReflect = "org.scala-lang" % "scala-reflect" % BuildSettings.buildScalaVersion
  val akka = "com.typesafe.akka" %% "akka-actor" % "2.3.7"
  val scalafx = "org.scalafx" %% "scalafx" % "8.0.20-R6" // https://github.com/scalafx/scalafx/releases
//  val sftp = "fr.janalyse" %% "janalyse-ssh" % "0.9.10" % "compile" // scala version too experimental as of 201311
  val sftp = "com.jcraft" % "jsch" % "0.1.51" //% "compile"
//  val h2 = "com.h2database" % "h2" % "1.4.182"
//  val squeryl = "org.squeryl" %% "squeryl" % "0.9.5-7"
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

//  val javaHome = Some("/Library/Java/JavaVirtualMachines/jdk1.7.0_55.jdk/Contents/Home") // TODO use above if environm var read

//  lazy val unmanagedListing = unmanagedJars in Compile += Attributed.blank(file(javaHome.get + "/jre/lib/jfxrt.jar"))

  // javafx ini for scalafx. needed only for package-javafx, in principle

//  jfxSettings

//  val myjfxsettings = jfxSettings ++ Seq(
//    JFX.mainClass := Some("sfsync.Main"),
//    JFX.devKit := JFX.jdk(javaHome.get),
//    JFX.addJfxrtToClasspath := true
//  )

//  no.vedaadata.sbtjavafx.JavaFXPlugin.packageJavaFxTask()

  lazy val sfsyncSettings = buildSettings ++ Seq(
    name := "SFSync",
    libraryDependencies ++= Seq(scala, akka, scalafx, sftp)//, h2, squeryl)
//    , unmanagedListing
  )

  lazy val myBuildInfoSettings = Seq(
    sourceGenerators in Compile <+= buildInfo,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion),
    buildInfoPackage := "sfsync"
  )

  val IntelliJexcludeFolders = Seq(
    ".idea", ".idea_modules", "src/main/resources/sfsync/HGVERSION.txt", "target"
  )
  lazy val root = Project(
    id = "SFSync",
    base = file("."),
    settings = sfsyncSettings
      ++ buildInfoSettings ++ myBuildInfoSettings
  ).settings(net.virtualvoid.sbt.graph.Plugin.graphSettings: _*)
//gone?    .settings(ideaExcludeFolders := IntelliJexcludeFolders)
}

*/