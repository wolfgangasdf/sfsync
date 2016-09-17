
# Introduction

SFSync is a file synchronization program. It compared the file modification time (mtime) and size to decide if a file is modified.
It keeps a local database to keep track of changes (remote / local).

It uses the following paradigm:

* `sync locations` are places in the local filesystem that are synchronized against some other place
    * if you want to sync against multiple places, create multiple sync locations for the same folder!
* `protocols` are protocols that are used to synchronize against the same remote location
    * you can e.g. use sftp and a samba mount in parallel, with different remote roots, while the same cache database is used.
* `subsets` are used to synchronize only part below a root. Often, there are only certain folders that I modify and I don't need to sync all folders everytime.
* You don't need an initial sync of everything below the root! I found this to be not acceptable in many cases.

## More information

* The log file `sfsynclog-<date>.txt` is in `/tmp` or in java.io.tmpdir
* SFSync has an option keep separate remote file date/timestamp/mtime for servers that can't modify file time (android & SSHelper without root etc.)
* sftp: use publickey or password-based authentification

## Status ##
File synchronization is a delicate thing. However, if you keep the Files list-view on the default setting "changes", only files are modified that you see in the list. You can review everything before pressing `Synchronize`.

* I use it since years without any data loss
* There is no sanity check before synchronization, so you can create the paradox to delete a folder but copy a child file. This will result in nice synchronization errors, but no data loss will happen.
* The routine that assigns the initial actions after comparison is tested on program startup. Check the code, I find this safe.
* But I can't be responsible for any data loss, of course.

# Getting started #

To run it, you need java >= 1.8

To build it, you need a java jdk >= 1.8 and sbt 0.13.

Run it: 

	sbt run

Package it:

    sbt dist

# Tutorial #

# Used frameworks #

* [Scala](http://www.scala-lang.org) and [Scala Build Tool](http://www.scala-sbt.org)
* [Scalafx](http://scalafx.org) as wrapper for [JavaFX](http://docs.oracle.com/javafx) for the graphical user interface
* [sbt-javafx](https://github.com/kavedaa/sbt-javafx) to create the runnable Reftool jar file
* [sbt-buildinfo](https://github.com/sbt/sbt-buildinfo) to access build information
* [sbt-appbundle](https://github.com/Sciss/sbt-appbundle) to create the mac app bundle
* a modified version of [universalJavaApplicationStub](https://github.com/tofi86/universalJavaApplicationStub) to launch Reftool on Mac 
* [SSHJ](https://github.com/hierynomus/sshj) to synchronize via sftp

# Contributors #

Contributions are of course very welcome, please contact me or use the standard methods.

# Maintainer #

wolfgang.loeffler@gmail.com

### License ###
[MIT](http://opensource.org/licenses/MIT)