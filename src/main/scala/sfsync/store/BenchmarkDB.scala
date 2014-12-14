package sfsync.store

import sfsync.CF

import scala.collection.mutable
import scalafx.beans.property.StringProperty


/*

see scalajavacollectionsperformance for more tests. result: just go with scala/java stuff. h2 > 20x slower
java treemap (sorted) is 2x slower than hashmap for me. fine

here: compare full syncentry object <> only small things. result: fine.

javatreemapobj put: 2.781246154  MB: 429.382216
javatreemapobj get: 1.653385772  MB: -0.021912
javatreemapobj upd: 1.489612098  MB: 1.52E-4
javatreemapobj put: 2.234432003  MB: 431.999464
javatreemapobj get: 1.523902344  MB: 2.0E-4
javatreemapobj upd: 1.454282525  MB: 2.0E-4
javatreemapobjfull put: 2.205669712  MB: 464.000888
javatreemapobjfull get: 1.653603882  MB: 1.52E-4
javatreemapobjfull upd: 1.705627857  MB: 1.52E-4
javatreemapobjfull put: 2.330513492  MB: 463.999416
javatreemapobjfull get: 1.555676103  MB: 2.0E-4
javatreemapobjfull upd: 1.681452701  MB: 2.0E-4

*/

object BenchmarkDB {

  val s1="123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"

  val nele = 1000000

  object Stats {
    def getUsedMemory: Long = {
      System.gc()
      Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
    }
    def stats[T](msg: String)(body: =>T) = { // for use via timed("time=") { body }
    val m0 = getUsedMemory
      val startNanos = System.nanoTime
      val r = body
      val stopNanos = System.nanoTime
      println(msg + ": " + (stopNanos - startNanos)/1e9 + "  MB: " + (getUsedMemory - m0)/1.0e6)
      r
    }
  }
  import Stats._


  class SE2(var l1: Long, var l2: Long, var l3: Long) {
    def getString = {
      s"aksjghsdklfghsdkfgjhsdkjfghsdkfjghsdkfghsdkfghsdlfghsdfg $l1  $l2  $l3"
    }
  }
  def testJavaTreeMapObj2() = { // there is no mutable scala TreeMap
  val hm = new java.util.TreeMap[String, SE2]()
    stats("javatreemapobj put") {
      for (ii <- 1 to nele) hm.put(s1 + ii, new SE2(ii, ii + 1, ii + 2))
    }
    stats("javatreemapobj get") {
      for (ii <- 1 to nele) hm.get(s1 + ii)
    }
    stats("javatreemapobj upd") {
      for (ii <- 1 to nele) hm.replace(s1 + ii, new SE2(ii+10, ii + 11, ii + 12))
    }
  }










  class SyncEntry(var action: Int,
                  var lTime: Long, var lSize: Long,
                  var rTime: Long, var rSize: Long,
                  var cTime: Long, var cSize: Long,
                  var isDir: Boolean, var relevant: Boolean,
                  var selected: Boolean = false,
                  var delete: Boolean = false
                   ) {
    var hasCachedParent = false // only used for folders!
    def sameTime(t1: Long, t2: Long) = Math.abs(t1 - t2) < 2000 // in milliseconds

    def status = new StringProperty(this, "status", CF.amap(action))
    def dformat = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
    def detailsLocal = new StringProperty(this, "detailsl",
      if (lSize != -1) dformat.format(new java.util.Date(lTime)) + "(" + lSize + ")" else "none")
    def detailsRemote = new StringProperty(this, "detailsr",
      if (rSize != -1) dformat.format(new java.util.Date(rTime)) + "(" + rSize + ")" else "none")
    def detailsCache = new StringProperty(this, "detailsc",
      if (cSize != -1) dformat.format(new java.util.Date(cTime)) + "(" + cSize + ")" else "none")
//    def isDir = path.endsWith("/")
    def isEqual = {
      if (isDir) {
        if (lSize != -1 && rSize != -1) true else false
      } else {
        if (lSize != -1 && lSize == rSize && sameTime(lTime, rTime)) true else false
      }
    }
    def isLeqC = {
      if (isDir) {
        if (lSize != -1 && cSize != -1) true else false
      } else {
        if (lSize != -1 && lSize == cSize && sameTime(lTime, cTime)) true else false
      }
    }
    def isReqC = {
      if (isDir) {
        if (rSize != -1 && cSize != -1) true else false
      } else {
        if (rSize != -1 && rSize == cSize && sameTime(rTime, cTime)) true else false
      }
    }
    def compareSetAction(newcache: Boolean) = {
      import sfsync.synchro.Actions._
      action = -9
      if (lSize == -1 && rSize == -1) { // cache only?
        action = A_CACHEONLY
      } else  if (isEqual) { // just equal?
        action = A_ISEQUAL
      } else if (cSize == -1) { // not in remote cache
        if (newcache) { // not equal, not in cache because cache new
          action = A_UNKNOWN
        } else { // not in cache but cache not new: new file?
          if (lSize != -1 && rSize == -1) action = A_USELOCAL // new local (cache not new)
          else if (lSize == -1 && rSize != -1) action = A_USEREMOTE // new remote (cache not new)
          else action = A_UNKNOWN // not in cache but both present
        }
      } else { // in cache, not equal
        if ( isLeqC && rSize == -1) action = A_RMLOCAL // remote was deleted (local still in cache)
        else if (lSize == -1 && isReqC ) action = A_RMREMOTE // local was deleted (remote still in cache)
        // both exist, as does fcache
        else if ( isLeqC && rTime > lTime) action = A_USEREMOTE // flocal unchanged, remote newer
        else if ( isReqC && lTime > rTime) action = A_USELOCAL // fremote unchanged, local newer
        else action = A_UNKNOWN // both changed and all other strange things that might occur
      }
      //  debug("CF: " + toString)
      assert(action != -9)
      //debug("iniaction: " + this.toString)
      this
    }
    override def toString = {s"[path=xx action=$action lTime=$lTime lSize=$lSize rTime=$rTime rSize=$rSize cTime=$cTime cSize=$cSize rel=$relevant"}
    def toStringNice = {
      s"""
     |Path: xx
     |Local : ${detailsLocal.value}
     |Remote: ${detailsRemote.value}
     |Cache : ${detailsCache.value} ($hasCachedParent)
    """.stripMargin
    }
  }

  def testJavaTreeMapObjFull() = {
  val hm = new java.util.TreeMap[String, SyncEntry]()
    stats("javatreemapobjfull put") {
      for (ii <- 1 to nele) hm.put(s1 + ii, new SyncEntry(ii, ii+1, ii+3, 1,2,3,4,false,false,false,false))
    }
    stats("javatreemapobjfull get") {
      for (ii <- 1 to nele) hm.get(s1 + ii)
    }
    stats("javatreemapobjfull upd") {
      for (ii <- 1 to nele) hm.replace(s1 + ii, new SyncEntry(ii, ii+1, ii+3, 1,2,3,4,false,false,false,false))
    }
  }

  def testScalaTreeMapObjFull() = {
    var hm = new scala.collection.immutable.TreeMap[String, SyncEntry]()
    stats("scalatreemapobjfull put") {
      for (ii <- 1 to nele) hm += (s1 + ii -> new SyncEntry(ii, ii+1, ii+3, 1,2,3,4,false,false,false,false))
    }
    stats("scalatreemapobjfull get") {
      for (ii <- 1 to nele) hm.get(s1 + ii)
    }
    stats("scalatreemapobjfull upd") {
      for (ii <- 1 to nele) hm += (s1 + ii -> new SyncEntry(ii, ii+1, ii+3, 1,2,3,4,false,false,false,false))
    }
  }

  def main(args: Array[String]): Unit = {
    testJavaTreeMapObj2()
    testJavaTreeMapObj2()
    testJavaTreeMapObjFull()
    testJavaTreeMapObjFull()
    testScalaTreeMapObjFull()
    testScalaTreeMapObjFull()
  }

}