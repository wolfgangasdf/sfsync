#!/bin/bash
# fix for mac java wrong filename encoding
export LC_CTYPE="UTF-8"

# -Xdock doesn't work with J7....
java -Xmx128m -Xdock:name="SFSync" -jar target/scala-2.10/sfsync_2.10.2-0.2/sfsync_2.10.2-0.2.jar
