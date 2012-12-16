//addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

// for scala 2.10, i need snapshot...

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.2.0-SNAPSHOT")

// TODO addSbtPlugin("no.vedaadata" %% "sbt-javafx" % "0.4.1-SNAPSHOT")

addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.8")