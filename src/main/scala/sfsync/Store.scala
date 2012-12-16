package sfsync.store

import sfsync.synchro.VirtualFile
import scalafx.{collections => sfxc}
import scalax.file.Path
import scalax.io.Line
import Tools._


object DBSettings {
  def dbpath = "/Unencrypted_Data/softwaredevelopment/sfsync-tmp/sfsyncsettings"
  def getSettingPath = DBSettings.dbpath + ".txt"
  def getLines = {
    val fff = Path.fromString(getSettingPath)
    if (!fff.exists) {
      fff.doCreateFile()
    }
    fff.lines(Line.Terminators.NewLine, includeTerminator = true)
  }
}

object Tools {
  // splits safely: at first comma
  def splitsetting(ss: String) : List[String] = {
    val commapos = ss.indexOf(",")
    val tag = ss.substring(0,commapos)
    val content = ss.substring(commapos+1).trim
    //    println(tag+" , " + content)
    List(tag,content)
  }
}

// list types:
// sfxc.ObservableBuffer[Server] needed for ListView. turns out to be useless since need [String].....
// mutable.ArrayBuffer[Server] is needed for db4o
class Config {
  var servers = new sfxc.ObservableBuffer[Server] // THIS does not work :-( if the servers is used by ListView, it crashes the DB.....
//  var servers = new mutable.ArrayBuffer[Server] //
  var currentServer = -1
  var currentFilter = 0
}

class Server {
  var name: String = "<new>"
  var id: String = new java.util.Date().getTime.toString
  var localFolder: String = ""
  var protocols = new sfxc.ObservableBuffer[Protocol]
  var currentProtocol = -1
  var subfolders = new sfxc.ObservableBuffer[SubFolder]
  var currentSubFolder = -1
  override def toString: String = name // used for listview
}

class Protocol {
  var name: String = "<new>"
  var protocoluri: String = ""
  var protocolbasefolder: String = ""
  override def toString: String = name
}

class SubFolder {
  var name: String = "<new>"
  var subfolder: String = ""
  override def toString: String = name
}


object Store {
  var config : Config = null

  def save() {
    println("-----------save " + config)
    val fff = Path.fromString(DBSettings.getSettingPath)
    fff.write("sfsyncsettingsversion,1\n")
    fff.append("servercurr," + config.currentServer + "\n")
    fff.append("currentFilter," + config.currentFilter + "\n")
    for (server <- config.servers) {
      println("server: " + server)
      fff.append("server," + server.name + "\n")
      fff.append("localfolder," + server.localFolder + "\n")
      fff.append("id," + server.id + "\n")
      fff.append("protocolcurr," + server.currentProtocol + "\n")
      for (proto <- server.protocols) {
        fff.append("protocol," + proto.name + "\n")
        fff.append("protocoluri," + proto.protocoluri + "\n")
        fff.append("protocolbasefolder," + proto.protocolbasefolder+ "\n")
      }
      fff.append("subfoldercurr," + server.currentSubFolder + "\n")
      for (subf <- server.subfolders) {
        fff.append("subfolder," + subf.name + "\n")
        fff.append("subfolderfolder," + subf.subfolder + "\n")
      }
    }
    println("-----------/save")
  }

  def load() {
    var lastserver: Server = null
    var lastprotocol: Protocol = null
    var lastsubfolder: SubFolder = null
    println("Store ini!!!!!!!!!!!!")
    val lines = DBSettings.getLines
    if (lines.size == 0) {
      println("no config file...")
      config = new Config
    } else {
      lines.foreach(lll => {
        val sett = splitsetting(lll.toString)
        sett(0) match {
          case "sfsyncsettingsversion" => {
            println("sett=<" + sett(1) + ">")
            if (!sett(1).equals("1")) sys.error("wrong settings version")
            config = new Config()
          }
          case "servercurr" => { config.currentServer = sett(1).toInt }
          case "currentFilter" => config.currentFilter = sett(1).toInt
          case "server" => {
            lastserver = new Server { name = sett(1) }
            config.servers.add(lastserver)
            println("added server " + lastserver)
          }
          case "localfolder" => { lastserver.localFolder = sett(1) }
          case "id" => { lastserver.id = sett(1) }
          case "protocolcurr" => { lastserver.currentProtocol = sett(1).toInt }
          case "protocol" => {
            lastprotocol = new Protocol { name = sett(1) }
            lastserver.protocols.add(lastprotocol)
          }
          case "protocoluri" => {lastprotocol.protocoluri = sett(1)}
          case "protocolbasefolder" => {lastprotocol.protocolbasefolder = sett(1)}
          case "subfoldercurr" => { lastserver.currentSubFolder = sett(1).toInt }
          case "subfolder" => {
            lastsubfolder = new SubFolder { name = sett(1) }
            lastserver.subfolders.add(lastsubfolder)
          }
          case "subfolderfolder" => {lastsubfolder.subfolder = sett(1)}
          case _ => {println("unknown tag in config file: <" + sett(0) + ">")}
        }
      })
    }
  }

  def dumpConfig() {
    println("--------------dumpconfig")
    for (server <- config.servers) {
      println("server: " + server + " currprot=" + server.currentProtocol + " currsf=" + server.currentSubFolder)
      server.protocols.foreach( proto => println("  proto: " + proto))
      server.subfolders.foreach( sf => println("  subfolder: " + sf) )
    }
    println("--------------/dumpconfig")
  }

  // Load config
  load()
}

object Cache {
  import collection.mutable.ListBuffer
  var cache: ListBuffer[VirtualFile] = null
  def getCacheFilename(name: String) = DBSettings.dbpath + "-cache" + name + ".txt"
  def loadCache(name: String) : ListBuffer[VirtualFile] = {
    cache = new ListBuffer[VirtualFile]()
    val fff = Path.fromString(getCacheFilename(name))
    if (!fff.exists) {
      println("create cache file!")
      fff.doCreateFile()
    }
    val lines = fff.lines(Line.Terminators.NewLine, includeTerminator = true)
    lines.foreach(lll => {
      var sett = splitsetting(lll.toString)
      val vf = new VirtualFile
      vf.modTime = sett(0).toLong
      sett = splitsetting(sett(1))
      vf.isDir = sett(0).toInt
      sett = splitsetting(sett(1))
      vf.size = sett(0).toLong
      vf.path = sett(1) // this is safe, also commas in filename ok
      cache += vf
    })
    println("loaded cache file!")
    cache
  }
  def remove(vf: VirtualFile) {
    if (cache.contains(vf)) {
      cache -= vf
      println(" removed from cache " + vf)
    } else {
      println(" error: cache doesn't contain " + vf)
    }
  }

  def addupdate(vf: VirtualFile) {
    val vfs = cache.filter(p => p.path == vf.path)
    println(" addup: found " + vfs)
    cache --= vfs
    cache += vf
    val vfs2 = cache.filter(p => p.path == vf.path)
    println(" after: found " + vfs2)
  }

  def saveCache(name: String) {
    val fff = Path.fromString(getCacheFilename(name))
    if (fff.exists) {
      fff.delete(force = true)
    }
    fff.doCreateFile()
    for (cf <- cache) {
      println("  savecache: " + cf)
      fff.append("" + cf.modTime + "," + cf.isDir + "," + cf.size + "," + cf.path + "\n")
    }
    println("***** cache saved!")
  }

  def clearCache(name: String) {
    val fff = Path.fromString(getCacheFilename(name))
    if (fff.exists) {
      fff.delete(force = true)
    }
  }

}