package sfsync.synchro

import sfsync.store.Cache
import sfsync.util.{Logging, StopWatch}
import sfsync.synchro.Actions._


object Actions {
  val A_UNCHECKED = -99
  val A_UNKNOWN = -1
  val A_ISEQUAL = 0
  val A_USELOCAL = 1
  val A_USEREMOTE = 2
  val A_MERGE = 3
  val A_RMLOCAL = 4
  val A_RMREMOTE = 5
  val A_CACHEONLY = 6
  val A_RMBOTH = 7
  val A_SYNCERROR = 8
  val A_SKIP = 9
  val ALLACTIONS = List(-99,-1,0,1,2,3,4,5,6,7,8,9)
}

object Comparison extends Logging {
  def getParentFolder(path: String) = {
    path.split("/").dropRight(1).mkString("/") + "/"
  }
  // compare, update database entries only.
  def compareSyncEntries(): Boolean = {
    val swse = new StopWatch
    // q contains all entries to be checked

    // to avoid first full sync: mark all folders that are subfolders of already synced folder
    // update hasCachedParent for all folders not in cache and check if it has parent folder that has been synced before in current set
    Cache.cache.iterate((it, path, se) => {
      if (se.relevant && se.isDir) {
        var tmpf = path
        var haveCachedParentDir = false
        var doit = true
        while (!haveCachedParentDir && doit) {
          tmpf = getParentFolder(tmpf)
          if (tmpf != "/") {
            if (Cache.cache.containsKey(tmpf))
              if (Cache.cache.get(tmpf).cSize != -1) haveCachedParentDir = true
          } else {
            doit = false
          }
        }
        se.hasCachedParent = haveCachedParentDir
        se.compareSetAction(newcache = !haveCachedParentDir) // compare
      }
    })
    debug("TTT a took = " + swse.getTimeRestart)

    // iterate over all folders that are cacheed
    Cache.cache.iterate((it, path, se) => {
      if (se.relevant && se.isDir && se.cSize != -1) {
        se.hasCachedParent = true
        se.compareSetAction(newcache = false) // compare
      }
    })
    debug("TTT b took = " + swse.getTimeRestart)

    // iterate over the rest: all files.
    Cache.cache.iterate((it, path, se) => {
      if (se.relevant && !se.isDir) {
        if (se.cSize == -1) { // only get parent folder for unknown files, faster!
        val parent = getParentFolder(path)
          if (!Cache.cache.get(parent).hasCachedParent) {
            se.compareSetAction(newcache = true) // test
          } else {
            se.compareSetAction(newcache = false)
          }
        } else {
          se.compareSetAction(newcache = false)
        }
      }
    })
    debug("TTT c took = " + swse.getTimeRestart)

    // iterate over all folders that will be deleted: check that other side is not modified below
    Cache.cache.iterate((it, path, se) => {
      if (se.relevant && se.isDir && List(A_RMLOCAL, A_RMREMOTE).contains(se.action)) {
        var fishy = false
        Cache.cache.iterate((it2, path2, se2) => {
          if (se.relevant && path2.startsWith(path) && se2.action != se.action) {
            fishy = true
          }
        })
        if (fishy) {
          Cache.cache.iterate((it2, path2, se2) => {
            if (se.relevant && path2.startsWith(path)) {
              se2.action = A_UNKNOWN
            }
          })
        }
      }
    })
    debug("TTT d took = " + swse.getTimeRestart)

    // return true if changes
    var res = false
    Cache.cache.iterate((it, path, se) => if (se.relevant && se.action != A_ISEQUAL) res = true)
    debug("TTT e took = " + swse.getTimeRestart)
    res
  }
}
