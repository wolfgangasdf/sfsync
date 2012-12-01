package sfsync.synchro

class TransferProtocol (
  var uri: String,
  var basefolder: String
)

class ComparedFile(flocal: VirtualFile, fremote: VirtualFile, fcache: VirtualFile) {
  val A_UNKNOWN = -1
  val A_NOTHING = 0
  val A_USELOCAL = 1
  val A_USEREMOTE = 2
  val A_MERGE = 3
  val A_RMLOCAL = 4
  val A_RMREMOTE = 5
  var action: Int = -9

  def isSynced() = flocal.equals(fremote)
  def isLocalModified = {}
  def isRemoteModified = {}
  override def toString: String = "A=" + action + " local:" + flocal + " remote:" + fremote + " cache:" + fcache

  // init with best guess
  if (flocal == fremote) { // just equal?
    action = A_NOTHING
  } else if (flocal == null || fremote == null) { // one was deleted, which?
    if (fcache==null) {
      action = A_UNKNOWN // unknown which was deleted
    } else {
      if (flocal==null && fremote==fcache) {
        action = A_RMREMOTE
      } else if (fremote==null && flocal==fcache) {
        action = A_RMLOCAL
      } else {
        action = A_UNKNOWN // one was deleted and the other modified: don't know
      }
    }
  } else { // both present, one is modified
    action = A_UNKNOWN
    if (flocal == fcache && fcache != null) { // no local modification
      if (fremote.modTime>fcache.modTime)
        action = A_USEREMOTE // only if remote newer than cache, else unknown
    } else if (fremote == fcache && fcache != null) { // local mod, remote not modified
      if (flocal.modTime > fcache.modTime)  // only if local newer than cache, else unknown
        action = A_USELOCAL
    } else { // item not in cache but both present
      if (flocal.modTime>fremote.modTime)
        action = A_USELOCAL
      else
      action = A_USEREMOTE
    }
  }
  assert(action != -9)
}

import util.Logging
import sfsync.store.Cache

class Profile  (
                localFolder: String,
                protocol: TransferProtocol,
                subfolder: String,
                id: String
                ) extends Logging {
  def compare() : scalafx.collections.ObservableBuffer[ComparedFile] = {
    var comparedfiles = scalafx.collections.ObservableBuffer[ComparedFile]()
    var cache = Cache.loadCache(id)
    // test local conn
    var local = new LocalConnection {
      basePath = localFolder
    }
    var remote = new LocalConnection {
      basePath = protocol.basefolder
    }
    debug("***********************local")
    var locall = local.listRecursively(subfolder)
    locall.foreach(vf => debug(vf))
    debug("***********************remote")
    var remotel = remote.listRecursively(subfolder)
    remotel.foreach(vf => debug(vf))
    debug("***********************cache")
    cache.foreach(vf => debug(vf))
    debug("***********************")

    locall.foreach(lf => {
      val cachef = cache.find(x => x.path == lf.path).getOrElse(null)
      val remotef = remotel.find(x => x.path == lf.path).getOrElse(null)
      remotel -= remotef
      comparedfiles += new ComparedFile(lf, remotef, cachef)
    })
    // add remaing remote-only files
    remotel.foreach(vf => {
      val cachef = cache.find(x => x.path == vf.path).getOrElse(null)
      comparedfiles += new ComparedFile(null, vf,cachef)
    })
    debug("***********************compfiles")
    comparedfiles.foreach(cf => println(cf))
    comparedfiles
  }

  def synchronize(cfs: scalafx.collections.ObservableBuffer[ComparedFile]) {
    for (cf <- cfs) {
      println("***** cf:" + cf)
      if (cf.action == cf.A_USELOCAL) {

      }

    }
  }
}

