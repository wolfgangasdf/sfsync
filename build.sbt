import _root_.de.sciss.sbt.appbundle.AppBundlePlugin.appbundle

// sbt {run,packageJavaFX} don't work if only Build.scala is used

////////////////// sbt-javafx for packaging
jfxSettings

JFX.mainClass := Some("sfsync.Main")

JFX.devKit := JFX.jdk(System.getenv("JAVA_HOME"))

/////////////// mac app bundle via sbt-appbundle
seq(appbundle.settings: _*)

appbundle.name := "Sfsync"

appbundle.javaVersion := "1.8*"

appbundle.icon := Some(file("src/deploy/macosx/sfsync.icns"))

appbundle.mainClass := JFX.mainClass.value

appbundle.executable := file("src/deploy/macosx/universalJavaApplicationStub")

/////////////// task to zip the jar for win,linux
lazy val tzip = TaskKey[Unit]("zip")
tzip := {
  println("zipping jar & libs...")
  val s = target.value + "/" + JFX.artifactBaseNameValue.value + "-win-linux.zip"
  IO.zip(
    Path.allSubpaths(new File(crossTarget.value + "/" + JFX.artifactBaseNameValue.value)).
      filterNot(_._2.endsWith(".html")).filterNot(_._2.endsWith(".jnlp")), new File(s))
  println("==> created windows & linux zip: " + s)
}
tzip <<= tzip.dependsOn(JFX.packageJavaFx)

/////////////// task to zip the mac app bundle
lazy val tzipmac = TaskKey[Unit]("zipmac")
tzipmac := {
  println("zipping mac app bundle...")
  val s = target.value + "/" + appbundle.name.value + "-mac.zip"
  IO.zip(Path.allSubpaths(new File(target.value + "/" + appbundle.name.value + ".app")), new File(s))
  println("==> created mac app zip: " + s)
}
tzipmac <<= tzipmac.dependsOn(appbundle.appbundle)

/////////////// task to do all at once
lazy val tdist = TaskKey[Unit]("dist")
tdist := println("Created sfsync distribution files!")
tdist <<= tdist.dependsOn(tzipmac, tzip)

