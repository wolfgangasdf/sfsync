package sfsync.synchro

class TransferProtocol (
  var name: String,
  var conntype: ConnType.Value,
  var basefolder: String

)

class ComparedFile(flocal: VirtualFile, fremote: VirtualFile, fcache: VirtualFile) {
  val A_UNKNOWN = -1
  val A_NOTHING = 0
  val A_USELOCAL = 1
  val A_USEREMOTE = 2
  val A_MERGE = 3
  var action: Int = A_UNKNOWN

  def isSynced() = flocal.equals(fremote)
  def isLocalModified = {}
  def isRemoteModified = {}
  override def toString: String = "A=" + action + " local:" + flocal + " remote:" + fremote + " cache:" + fcache

  // init with best guess
  if (flocal == fremote) { // just equal?
    action = A_NOTHING
  } else if (flocal == fcache && fcache != null) { // no local modification
    if (fremote.modTime > fcache.modTime) { // if remote not newer, leave unknown!
      action = A_USEREMOTE
    }
  } else if (fremote == fcache && fcache != null) { // local mod, remote not modified
    if (flocal.modTime > fcache.modTime) { // if local not newer, leave unknown!
      action = A_USELOCAL
    }
  } else { // item not in cache
    if (fremote != null) {
      action = A_USEREMOTE // since flocal has to be null
    } else {
      action = A_USELOCAL // since fremote has to be null
    }
  }

}

import util.Logging
import collection.mutable.ListBuffer

class Profile  (
                val name: String,
                localFolder: String,
                protocol: TransferProtocol,
                subfolder: String
                ) extends Logging {
  def synchronize() : List[ComparedFile] = {
    var comparedfiles = ListBuffer[ComparedFile]()
    var cache = ListBuffer[VirtualFile]()
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

    locall.foreach(lf => {
      debug("lf=" + lf)
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
    comparedfiles.toList
    // TODO: let user decide what to do and update comparedfiles

    // TODO: do action and update cache (also delete not anymore existing files for this subfolder!)
    // or do this in GUI? then nice update possible
  }
}

