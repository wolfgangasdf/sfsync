
// sbt {run,packageJavaFX} don't work if only Build.scala is used

////////////////// sbt-javafx for packaging
jfxSettings

JFX.mainClass := Some("sfsync.Main")

JFX.devKit := JFX.jdk(System.getenv("JAVA_HOME"))

