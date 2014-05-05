package sfsync

import synchro.Actions._
import synchro.CompareStuff
import sfsync.store.{SyncEntry, MySchema, CacheDB}
import org.squeryl.PrimitiveTypeMode._
import scala.collection.mutable.ArrayBuffer

object Checks {
  def CheckComparedFile() {
    val mod0: Long = 1234
    val mod1: Long = 1235
    val mod2: Long = 1236
    val s0: Long = 1000
    val s1: Long = 1001
    class CheckEntry(val expectedAction: Int, val se: SyncEntry)

    val dbexists = CacheDB.connectDB("checks")
    if (dbexists) CacheDB.clearDB()
    transaction {
      // setup cachedb
      // stuff in existing & synced subfolder
      val ces = new ArrayBuffer[CheckEntry]
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf1/", A_UNCHECKED, mod0, s0, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf1/fileequal", A_UNCHECKED, mod0, s0, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_USEREMOTE, new SyncEntry("/sf1/fileremmod-t", A_UNCHECKED, mod0, s0, mod1, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf1/fileremmod-s(strange)", A_UNCHECKED, mod0, s0, mod0, s1, mod0, s0, true))
      ces += new CheckEntry(A_USELOCAL, new SyncEntry("/sf1/filelocmod-t", A_UNCHECKED, mod1, s0, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf1/filelocmod-s(strange)", A_UNCHECKED, mod0, s1, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_USELOCAL, new SyncEntry("/sf1/filelocnew", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true))
      ces += new CheckEntry(A_USEREMOTE, new SyncEntry("/sf1/fileremnew", A_UNCHECKED, -1, -1, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_USELOCAL, new SyncEntry("/sf1/foldlocnew/", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true))
      ces += new CheckEntry(A_USEREMOTE, new SyncEntry("/sf1/foldremnew/", A_UNCHECKED, -1, -1, mod0, s0, -1, -1, true))
      // old checks
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf0/", A_UNCHECKED, mod0, 0, mod0, 0, mod0, 0, true))
      ces += new CheckEntry(A_CACHEONLY, new SyncEntry("/sf0/file01", A_UNCHECKED, -1, -1, -1, -1, mod0, s0, true)) // cache only?
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf0/file02", A_UNCHECKED, mod0, s0, mod0, s0, mod0, s0, true)) // just equal?
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf0/file03", A_UNCHECKED, mod0, s0, mod1, s0, -1, -1, true)) // not equal and not in cache. unknown!
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf0/file04", A_UNCHECKED, mod0, s1, mod0, s0, -1, -1, true)) // not equal and not in cache. unknown!

      // new path with file in synced subfolder
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf2/", A_UNCHECKED, mod0, s0, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_USEREMOTE, new SyncEntry("/sf2/foldremnew/", A_UNCHECKED, -1, -1, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_USEREMOTE, new SyncEntry("/sf2/foldremnew/file", A_UNCHECKED, -1, -1, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_USELOCAL, new SyncEntry("/sf2/foldlocnew/", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true))
      ces += new CheckEntry(A_USELOCAL, new SyncEntry("/sf2/foldlocnew/file", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true))
      // new path with file in NOT synced subfolder
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf3/", A_UNCHECKED, -1, -1, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf3/foldremnew/", A_UNCHECKED, -1, -1, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf3/foldremnew/file", A_UNCHECKED, -1, -1, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf4/", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf4/foldlocnew/", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf4/foldlocnew/file", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true))

      // unsynced subfolder, some equal and unequal files
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf5/", A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf5/fold/", A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf5/fold/file1", A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf5/fold/file2", A_UNCHECKED, mod0, s0, mod1, s1, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf5/fold/file3", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true)) // deleted or not?

      // same with synced subfolder
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf6/", A_UNCHECKED, mod0, s0, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf6/fold/", A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf6/fold/file1", A_UNCHECKED, mod0, s0, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf6/fold/file2", A_UNCHECKED, mod0, s0, mod1, s1, -1, -1, true))
      ces += new CheckEntry(A_USELOCAL, new SyncEntry("/sf6/fold/file3", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true)) // deleted or not?

      // old checks
      ces += new CheckEntry(A_ISEQUAL, new SyncEntry("/sf7ca/", A_UNCHECKED, mod0, s0, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_USELOCAL, new SyncEntry("/sf7ca/filenewloc", A_UNCHECKED, mod0, s0, -1, -1, -1, -1, true))
      ces += new CheckEntry(A_USEREMOTE, new SyncEntry("/sf7ca/filenewrem", A_UNCHECKED, -1, -1, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf7ca/filenewerlocnocache", A_UNCHECKED, mod1, s0, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf7ca/filenewerremnocache", A_UNCHECKED, mod0, s0, mod1, s0, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf7ca/filesizediffnocache", A_UNCHECKED, mod0, s0, mod0, s1, -1, -1, true))
      // with cache
      ces += new CheckEntry(A_RMLOCAL, new SyncEntry("/sf7ca/filesremdelcache", A_UNCHECKED, mod0, s0, -1, -1, mod0, s0, true))
      ces += new CheckEntry(A_RMREMOTE, new SyncEntry("/sf7ca/fileslocdelcache", A_UNCHECKED, -1, -1, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf7ca/filesremdellocmodcacheold", A_UNCHECKED, mod1, s1, -1, -1, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf7ca/fileslocdelremmodcacheold", A_UNCHECKED, -1, -1, mod1, s1, mod0, s0, true))
      ces += new CheckEntry(A_USEREMOTE, new SyncEntry("/sf7ca/filesremmodcache", A_UNCHECKED, mod0, s0, mod1, s0, mod0, s0, true))
      ces += new CheckEntry(A_USELOCAL, new SyncEntry("/sf7ca/fileslocmodcache", A_UNCHECKED, mod1, s0, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf7ca/filesremmodlocmodcache", A_UNCHECKED, mod2, s0, mod1, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf7ca/fileslocmodremmodcache", A_UNCHECKED, mod1, s0, mod2, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf7ca/filessizeloccache", A_UNCHECKED, mod0, s1, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf7ca/filessizeremcache", A_UNCHECKED, mod0, s0, mod0, s1, mod0, s0, true))

      // cached, then delete local dir, but added/changed files remote: must all be ?
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf8/", A_UNCHECKED, -1, -1, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf8/filedelloc", A_UNCHECKED, -1, -1, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf8/fileaddrem", A_UNCHECKED, -1, -1, mod0, s0, -1, -1, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf8a/", A_UNCHECKED, -1, -1, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf8a/filedelloc", A_UNCHECKED, -1, -1, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf8a/filemodrem", A_UNCHECKED, mod0, s0, mod1, s1, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf8c/", A_UNCHECKED, -1, -1, mod0, s0, mod0, s0, true))
      ces += new CheckEntry(A_UNKNOWN, new SyncEntry("/sf8c/filemodrem", A_UNCHECKED, -1, -1, mod1, s1, mod0, s0, true))


      // insert stuff
      ces.foreach(ce => MySchema.files.insert(ce.se))

      println("**** initial:")
      MySchema.files.foreach(se => println(se.toString))

      CompareStuff.compareSyncEntries()

      // check if ok
      println("**** checks:")
      var fail = false
      ces.foreach(ce => {
        val senew = MySchema.files.where(se => se.path === ce.se.path).head
        println(
          (if (senew.action == ce.expectedAction) "" else {fail = true; "XX"})
            +  "[%s]: action=[%s] (expected [%s])".format(senew.path, CF.amap(senew.action), CF.amap(ce.expectedAction)))
      })

      assert(!fail)
    }
  }
}
