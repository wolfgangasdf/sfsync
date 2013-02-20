package sfsync

import synchro.VirtualFile
import synchro.Actions._

object Checks {
  def CheckComparedFile() {
    val mod0: Long = 1234
    val mod1: Long = 1235
    val mod2: Long = 1236
    val s0: Long = 1000
    val s1: Long = 1001

//    val isdir = 0 // only checks for files.
//    val lfp0m0s0 = new VirtualFile("path0",mod0,s0,isdir)
//    val rfp0m0s0 = new VirtualFile("path0",mod0,s0,isdir)
//    val cfp0m0s0 = new VirtualFile("path0",mod0,s0,isdir)
//    val lfp0m1s0 = new VirtualFile("path0",mod1,s0,isdir)
//    val rfp0m1s0 = new VirtualFile("path0",mod1,s0,isdir)
//    val lfp0m2s0 = new VirtualFile("path0",mod2,s0,isdir)
//    val rfp0m2s0 = new VirtualFile("path0",mod2,s0,isdir)
//    val lfp0m0s1 = new VirtualFile("path0",mod0,s1,isdir)
//    val rfp0m0s1 = new VirtualFile("path0",mod0,s1,isdir)
//    val cfp0m0s1 = new VirtualFile("path0",mod0,s1,isdir)
//    assert((new ComparedFile(null, null, cfp0m0s0)).action == A_CACHEONLY) // cache only?
//    assert((new ComparedFile(lfp0m0s0, rfp0m0s0, cfp0m0s0)).action == A_NOTHING) // just equal?
//    // not in remote cache
//    assert((new ComparedFile(lfp0m0s0, rfp0m0s1, null,true)).action == A_UNKNOWN) // not equal and not in cache. unknown!
//    assert((new ComparedFile(lfp0m1s0, rfp0m0s0, null,true)).action == A_UNKNOWN) // not equal and not in cache. unknown!
//    assert((new ComparedFile(lfp0m0s0, null, null)).action == A_USELOCAL) // new local (cache not new)
//    assert((new ComparedFile(null, rfp0m0s0, null)).action == A_USEREMOTE) // new remote (cache not new)
//    assert((new ComparedFile(lfp0m1s0, rfp0m0s0, null)).action == A_UNKNOWN) // local newer (no cache info)
//    assert((new ComparedFile(lfp0m0s0, rfp0m1s0, null)).action == A_UNKNOWN) // remote newer (no cache info)
//    assert((new ComparedFile(lfp0m0s0, rfp0m0s1, null)).action == A_UNKNOWN) // same moddate, different size...
//    // in cache
//    assert((new ComparedFile(lfp0m0s0, null, cfp0m0s0)).action == A_RMLOCAL) // remote was deleted (local still in cache)
//    assert((new ComparedFile(null, rfp0m0s0, cfp0m0s0)).action == A_RMREMOTE) // local was deleted (remote still in cache)
//    assert((new ComparedFile(lfp0m0s0, rfp0m1s0, cfp0m0s0)).action == A_USEREMOTE) // flocal unchanged, remote newer
//    assert((new ComparedFile(lfp0m1s0, rfp0m0s0, cfp0m0s0)).action == A_USELOCAL) // fremote unchanged, local newer
//    assert((new ComparedFile(lfp0m2s0, rfp0m1s0, cfp0m0s0)).action == A_UNKNOWN) // both newer than cache but local newer than remote
//    assert((new ComparedFile(lfp0m1s0, rfp0m2s0, cfp0m0s0)).action == A_UNKNOWN) // both newer than cache but remote newer than local
//
//    assert((new ComparedFile(lfp0m0s0, rfp0m0s1, cfp0m0s0)).action == A_UNKNOWN) // strange
//    assert((new ComparedFile(lfp0m0s1, rfp0m0s0, cfp0m0s1)).action == A_UNKNOWN) // strange

    // TODO
    println("NOT IMPL passed CheckComparedFile!")

  }

}
