name := "sfsync"

version := "0.2" // change also sfsync.Main.VERSION

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-feature", "-deprecation")

////////// intellij doesn't like same output path
// doesntwork target in Compile <<= baseDirectory(_ / "targetsbt") 

/////////////// akka actors (should work without this but doesn't)
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"

////////////////// for compile with javafx

unmanagedJars in Compile += Attributed.blank(file(System.getenv("JAVA_HOME") + "/jre/lib/jfxrt.jar"))

mainClass in (Compile, run) := Some("sfsync.Main")

////////////////// locally compiled scalafx!
libraryDependencies += "org.scalafx" %% "scalafx-core" % "1.0-SNAPSHOT"

////////////////// sftp
resolvers += "JAnalyse Repository" at "http://www.janalyse.fr/repository/"

libraryDependencies += "fr.janalyse" %% "janalyse-ssh" % "0.9.5-b3" % "compile"

////////////////// squeryl & derby
libraryDependencies += "com.h2database" % "h2" % "1.3.170"

libraryDependencies += "org.squeryl" %% "squeryl" % "0.9.5-6"

////////////////// sbt-javafx (local compiled)
jfxSettings

JFX.mainClass := Some("sfsync.Main")

JFX.devKit := JFX.jdk(System.getenv("JAVA_HOME"))

JFX.addJfxrtToClasspath := true


////////////////// old unused

////////////////// file io 
// libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.5.0-SNAPSHOT"

// libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.5.0-SNAPSHOT"


//libraryDependencies += "org.apache.derby" % "derby" % "10.9.1.0"

// add cross CrossVersion.full to have full scala version (with 'rc')
//libraryDependencies += "org.squeryl" %% "squeryl" % "0.9.5-4" cross CrossVersion.full


//db4o: not anymore
//externalResolvers += "source.db4o" at "http://source.db4o.com/maven/"

//libraryDependencies += "com.db4o" % "db4o-full-java5" % "8.1-SNAPSHOT"


////////////////// one-jar

//seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

//libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

//mainClass in oneJar := Some("sfsync.Main")

