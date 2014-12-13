# README #

## Introduction ##

SFSync is a file synchronization program. It uses different paradigms than http://jfilesync.sourceforge.net/ :

* `sync locations` are places in the local filesystem that are synchronized against some other place
    * if you want to sync against multiple places, create multiple sync locations for the same folder!
* `protocols` are protocols that are used to synchronize against the same remote location
    * you can e.g. use sftp and a samba mount in parallel, with different remote roots, while the same cache database is used.
* `subsets` are used to synchronize only part below a root. Often, there are only certain folders that I modify and I don't need to sync all everytime.
* You don't need an initial sync of everything below the root! I found this to be not acceptable in many cases.

## Current version ##

## Status ##
File synchronization is a delicate thing. However, if you keep the Files list-view on the default setting "changes", only files are modified that you see in the list. You can review everything before pressing `Synchronize`.

* I use it since more than a year without any data loss
* There is no sanity check before synchronization, so you can create the paradox to delete a folder but copy a child file. This will result in nice synchronization errors, but if your changes are intentionally, no data loss will happen.
* The routine that assigns the initial actions after comparison is tested on program startup. Check the code, I find this safe.
* But I can't be responsible for any data loss, of course.

### Getting started ###

To run it, you need java >= 1.8

To build it, you need a java jdk >= 1.8 and sbt 0.13.

To run it: 

	sbt run

To package it (and run via shell script, e.g., via quicksilver)

	rm -r target # wildcards in sfsync-run.sh
	sbt project packageJavafx
	./sfsync-run.sh

### Tutorial ###
to be written

### Used frameworks ###

* scala 2.11.4
* akka
* scalafx 8
* com.jcraft.jsch for sftp
* h2 database for cache
* squeryl

### Contributors ###

Contributions are of course very welcome, please contact me or use the standard methods.

### Maintainer ###

wolfgang.loeffler@gmail.com