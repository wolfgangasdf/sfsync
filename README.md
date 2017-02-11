
# Introduction

SFSync is a file synchronization program. It compared the file modification time (mtime) and size to decide if a file is modified.
It keeps a local database to keep track of changes (remote / local). It also works with Android devices without root via ssh (SSHelper etc) where the file attributes (time etc) can't be set.

It uses the following paradigm:

* `sync locations` are places in the local filesystem that are synchronized against some other place
    * if you want to sync against multiple places, create multiple sync locations for the same folder!
* `protocols` are protocols that are used to synchronize against the same remote location
    * you can e.g. use sftp and a samba mount in parallel, with different remote roots, while the same cache database is used.
* `subsets` are used to synchronize only part below a root. Often, there are only certain folders that I modify and I don't need to sync all folders everytime.
* You don't need an initial sync of everything! I found this to be not acceptable in many cases.

### More information

* The log file `sfsynclog-<date>.txt` is in `/tmp` or in java.io.tmpdir
* SFSync has an option keep separate remote file date/timestamp/mtime for servers that can't modify file time (android & SSHelper without root etc.)
* sftp: use publickey or password-based authentification (password stored in settings, hashed but not very secure)
* sftp: remote symbolic links (symlinks) are not 'followed'

### Status ###
File synchronization is a delicate thing. However, if you keep the Files list-view on the default setting "changes", only files are modified that you see in the list. You can review everything before pressing `Synchronize`.

* I use it since years without any data loss
* There is no sanity check before synchronization, so you can create the paradox to delete a folder but copy a child file. This will result in nice synchronization errors, but no data loss will happen.
* The routine that assigns the initial actions after file scan is tested on program startup. Check the code, I find this is safe.
* But I can't be responsible for any data loss, of course.

### How to use ###

* Get the [Java JRE](http://www.oracle.com/technetwork/java/javase/downloads/index.html) >= 8u101. Don't forget to untick the [crapware](https://www.google.com/search?q=java+crapware) installer, and/or [disable it permanently](https://www.java.com/en/download/faq/disable_offers.xml)!
* [Download the zip](https://bitbucket.org/wolfgang/sfsync/downloads) for Mac or (Windows, Linux), extract it somewhere and double-click the app (Mac) or
  jar file (Windows, Linux).

Everything should be self-explanatory (watch out for tooltips).

### How to develop, compile & package ###

* Get Java JDK >= 8u101
* check out the code (`hg clone ...` or download a zip)
* I use the free community version of [IntelliJ IDEA](https://www.jetbrains.com/idea/download/) with the scala
plugin for development, just import the project to get started.

Run Reftool from terminal and package it:

* Install the [Scala Build Tool](http://www.scala-sbt.org/)
* Compile and run manually: `sbt run`
* Package for all platforms: `sbt dist`. The resulting files are in `target/`

### Contributors ###

Contributions are of course very welcome, please contact me or use the standard methods.

### Used frameworks ###

* [Scala](http://www.scala-lang.org) and [Scala Build Tool](http://www.scala-sbt.org)
* [Scalafx](http://scalafx.org) as wrapper for [JavaFX](http://docs.oracle.com/javafx) for the graphical user interface
* [sbt-javafx](https://github.com/kavedaa/sbt-javafx) to create the runnable Reftool jar file
* [sbt-buildinfo](https://github.com/sbt/sbt-buildinfo) to access build information
* [sbt-appbundle](https://github.com/Sciss/sbt-appbundle) to create the mac app bundle
* a modified version of [universalJavaApplicationStub](https://github.com/tofi86/universalJavaApplicationStub) to launch Reftool on Mac 
* [SSHJ](https://github.com/hierynomus/sshj) to synchronize via sftp

### License ###
[MIT](http://opensource.org/licenses/MIT)