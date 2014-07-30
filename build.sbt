
// i use both this and build.scala.
// i did not want to figure out how to add this to build.scala

////////////////// sbt-javafx for packaging
jfxSettings

JFX.mainClass := Some("sfsync.Main")

JFX.devKit := JFX.jdk(System.getenv("JAVA_HOME"))

