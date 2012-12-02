name := "sfsync"

version := "0.1"

// scalafx does not work with scala 2.10 yet. WAIT
//scalaVersion := "2.10.0-RC1"
scalaVersion := "2.9.2"

//javaHome := Some(file("/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home"))

//libraryDependencies += "org.apache.derby" % "derby" % "10.9.1.0"

// add cross CrossVersion.full to have full scala version (with 'rc')
//libraryDependencies += "org.squeryl" %% "squeryl" % "0.9.5-4" cross CrossVersion.full

// locally compiled scalafx!
libraryDependencies += "org.scalafx" % "scalafx" % "1.0-SNAPSHOT"

// sftp
resolvers += "JAnalyse Repository" at "http://www.janalyse.fr/repository/"

libraryDependencies += "fr.janalyse" %% "janalyse-ssh" % "0.9.5-b3" % "compile"


//db4o: not anymore
//externalResolvers += "source.db4o" at "http://source.db4o.com/maven/"

//libraryDependencies += "com.db4o" % "db4o-full-java5" % "8.1-SNAPSHOT"


// file io
libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.1-seq"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.1-seq"
