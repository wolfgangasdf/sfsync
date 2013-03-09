#!/bin/bash
# fix for mac java wrong filename encoding
export LC_CTYPE="UTF-8"

java -Xmx128m -jar target/scala-2.10/sfsync_2.10.0-0.2/sfsync_2.10.0-0.2.jar
