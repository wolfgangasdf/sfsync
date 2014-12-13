# README #

## Introduction ##

SFSync is a file synchronization program. It uses different paradigms than http://jfilesync.sourceforge.net/ :
    * `sync locations` are places in the local filesystem that are synchronized against some other place
        * if you want to sync against multiple places, create multiple sync locations for the same folder!
    * `protocols` are protocols that are used to synchronize against the same remote location
        * you can e.g. use sftp and a samba mount in parallel, with different remote roots, while the same cache database is used.
    * `subsets` are used to synchronize only part below a root. Often, there are only certain folders that I modify and I don't need to sync all everytime.


Current version: 

### Getting started ###

To run it: 

	sbt run

To package it (and run via shell script, e.g., via quicksilver)

	rm -r target # wildcards in sfsync-run.sh
	sbt project packageJavafx
	./sfsync-run.sh


### Contributors ###

Contributions are of course very welcome, please contact me or use the standard methods.

### Maintainer ###

wolfgang.loeffler@gmail.com