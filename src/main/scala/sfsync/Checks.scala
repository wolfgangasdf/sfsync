package sfsync

import sfsync.synchro.Actions._
import sfsync.synchro.Comparison
import sfsync.store.{SyncEntry, Cache}

import scala.collection.mutable.ArrayBuffer

object Checks {
  def CheckComparedFile() {
    val mod0: Long = 12340000
    val mod1: Long = 12350000
    val mod2: Long = 12360000
    val s0: Long = 1000
    val s1: Long = 1001

    class CheckEntry(val expectedAction: Int, val path: String, val se: SyncEntry)

    Cache.iniCache()
    // setup cachedb
    // stuff in existing & synced subfolder
    val ces = new ArrayBuffer[CheckEntry]
    ces += new CheckEntry(A_ISEQUAL, "/sf1/", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, mod0, mod0, s0, true, true))
    ces += new CheckEntry(A_ISEQUAL, "/sf1/fileequal", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_USEREMOTE, "/sf1/fileremmod-t", new SyncEntry(A_UNCHECKED, mod0, s0, mod1, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf1/fileremmod-s(strange)", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s1, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_USELOCAL, "/sf1/filelocmod-t", new SyncEntry(A_UNCHECKED, mod1, s0, mod0, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf1/filelocmod-s(strange)", new SyncEntry(A_UNCHECKED, mod0, s1, mod0, mod0, s0, mod0, s0, false, true))
    ces += new CheckEntry(A_USELOCAL, "/sf1/filelocnew", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, false, true))
    ces += new CheckEntry(A_USEREMOTE, "/sf1/fileremnew", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, -1, -1, -1, false, true))
    ces += new CheckEntry(A_USELOCAL, "/sf1/foldlocnew/", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, true, true))
    ces += new CheckEntry(A_USEREMOTE, "/sf1/foldremnew/", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, -1, -1, -1, true, true))

    // new checks if remote can't set date
    ces += new CheckEntry(A_ISEQUAL, "/sf1/filexequal", new SyncEntry(A_UNCHECKED, mod0, s0, mod1, s0, mod0, mod1, s0, false, true))
    ces += new CheckEntry(A_RMREMOTE, "/sf1/filexrmrem", new SyncEntry(A_UNCHECKED, -1, -1, mod1, s0, mod0, mod1, s0, false, true))

    // old checks
    ces += new CheckEntry(A_ISEQUAL, "/sf0/", new SyncEntry(A_UNCHECKED, mod0, 0, mod0, 0, mod0, 0, 0, true, true))
    ces += new CheckEntry(A_CACHEONLY, "/sf0/file01", new SyncEntry(A_UNCHECKED, -1, -1, -1, -1, mod0, mod0, s0, false, true)) // cache only?
    ces += new CheckEntry(A_ISEQUAL, "/sf0/file02", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, mod0, mod0, s0, false, true)) // just equal?
    ces += new CheckEntry(A_UNKNOWN, "/sf0/file03", new SyncEntry(A_UNCHECKED, mod0, s0, mod1, s0, -1, -1, -1, false, true)) // not equal and not in cache. unknown!
    ces += new CheckEntry(A_UNKNOWN, "/sf0/file04", new SyncEntry(A_UNCHECKED, mod0, s1, mod0, s0, -1, -1, -1, false, true)) // not equal and not in cache. unknown!

    // new path with file in synced subfolder
    ces += new CheckEntry(A_ISEQUAL, "/sf2/", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, mod0, mod0, s0, true, true))
    ces += new CheckEntry(A_USEREMOTE, "/sf2/foldremnew/", new SyncEntry(A_UNCHECKED, -1, -1, mod0, mod0, s0, -1, -1, true, true))
    ces += new CheckEntry(A_USEREMOTE, "/sf2/foldremnew/file", new SyncEntry(A_UNCHECKED, -1, -1, mod0, mod0, s0, -1, -1, false, true))
    ces += new CheckEntry(A_USELOCAL, "/sf2/foldlocnew/", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, true, true))
    ces += new CheckEntry(A_USELOCAL, "/sf2/foldlocnew/file", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, false, true))
    // new path with file in NOT synced subfolder
    ces += new CheckEntry(A_UNKNOWN, "/sf3/", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, -1, -1, -1, true, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf3/foldremnew/", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, -1, -1, -1, true, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf3/foldremnew/file", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, -1, -1, -1, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf4/", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, true, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf4/foldlocnew/", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, true, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf4/foldlocnew/file", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, false, true))

    // unsynced subfolder, some equal and unequal files
    ces += new CheckEntry(A_ISEQUAL, "/sf5/", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, -1, true, true))
    ces += new CheckEntry(A_ISEQUAL, "/sf5/fold/", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, -1, true, true))
    ces += new CheckEntry(A_ISEQUAL, "/sf5/fold/file1", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, -1, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf5/fold/file2", new SyncEntry(A_UNCHECKED, mod0, s0, mod1, s1, -1, -1, -1, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf5/fold/file3", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, false, true)) // deleted or not?

    // same with synced subfolder
    ces += new CheckEntry(A_ISEQUAL, "/sf6/", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, mod0, mod0, s0, true, true))
    ces += new CheckEntry(A_ISEQUAL, "/sf6/fold/", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, -1, true, true))
    ces += new CheckEntry(A_ISEQUAL, "/sf6/fold/file1", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, -1, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf6/fold/file2", new SyncEntry(A_UNCHECKED, mod0, s0, mod1, s1, -1, -1, -1, false, true))
    ces += new CheckEntry(A_USELOCAL, "/sf6/fold/file3", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, false, true)) // deleted or not?

    // old checks
    ces += new CheckEntry(A_ISEQUAL, "/sf7ca/", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s0, mod0, mod0, s0, true, true))
    ces += new CheckEntry(A_USELOCAL, "/sf7ca/filenewloc", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, -1, -1, -1, false, true))
    ces += new CheckEntry(A_USEREMOTE, "/sf7ca/filenewrem", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, -1, -1, -1, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf7ca/filenewerlocnocache", new SyncEntry(A_UNCHECKED, mod1, s0, mod0, s0, -1, -1, -1, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf7ca/filenewerremnocache", new SyncEntry(A_UNCHECKED, mod0, s0, mod1, s0, -1, -1, -1, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf7ca/filesizediffnocache", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s1, -1, -1, -1, false, true))
    // with cache
    ces += new CheckEntry(A_RMLOCAL, "/sf7ca/filesremdelcache", new SyncEntry(A_UNCHECKED, mod0, s0, -1, -1, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_RMREMOTE, "/sf7ca/fileslocdelcache", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf7ca/filesremdellocmodcacheold", new SyncEntry(A_UNCHECKED, mod1, s1, -1, -1, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf7ca/fileslocdelremmodcacheold", new SyncEntry(A_UNCHECKED, -1, -1, mod1, s1, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_USEREMOTE, "/sf7ca/filesremmodcache", new SyncEntry(A_UNCHECKED, mod0, s0, mod1, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_USELOCAL, "/sf7ca/fileslocmodcache", new SyncEntry(A_UNCHECKED, mod1, s0, mod0, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf7ca/filesremmodlocmodcache", new SyncEntry(A_UNCHECKED, mod2, s0, mod1, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf7ca/fileslocmodremmodcache", new SyncEntry(A_UNCHECKED, mod1, s0, mod2, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf7ca/filessizeloccache", new SyncEntry(A_UNCHECKED, mod0, s1, mod0, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf7ca/filessizeremcache", new SyncEntry(A_UNCHECKED, mod0, s0, mod0, s1, mod0, mod0, s0, false, true))

    // cached, then delete local dir, but added/changed files remote: must all be ?
    ces += new CheckEntry(A_UNKNOWN, "/sf8/", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, mod0, mod0, s0, true, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf8/filedelloc", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf8/fileaddrem", new SyncEntry(A_UNCHECKED, -1, -1, mod0, mod0, s0, -1, -1, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf8a/", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, mod0, mod0, s0, true, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf8a/filedelloc", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf8a/filemodrem", new SyncEntry(A_UNCHECKED, mod0, s0, mod1, s1, mod0, mod0, s0, false, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf8c/", new SyncEntry(A_UNCHECKED, -1, -1, mod0, s0, mod0, mod0, s0, true, true))
    ces += new CheckEntry(A_UNKNOWN, "/sf8c/filemodrem", new SyncEntry(A_UNCHECKED, -1, -1, mod1, s1, mod0, mod0, s0, false, true))


    // insert stuff
    ces.foreach(ce => Cache.cache.put(ce.path, ce.se))

//    println("**** initial:")
//    Cache.dumpAll()

    Comparison.compareSyncEntries()

    // check if ok
    println("**** checks:")
    var fail = false
    ces.foreach(ce => {
      val senew = Cache.cache.get(ce.path)
      println(
        (if (senew.action == ce.expectedAction) "" else {fail = true; "XX"})
          +  "[%s]: action=[%s] (expected [%s])".format(ce.path, CF.amap(senew.action), CF.amap(ce.expectedAction)))
    })

    assert(!fail)
  }
}
