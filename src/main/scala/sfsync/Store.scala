package sfsync.store

import sfsync.synchro.VirtualFile
import scalafx.{collections => sfxc}
import scalafx.beans.property._
import scalax.file.Path
import scalax.io.Line
import Tools._
import sfsync.Helpers._



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
  var currentServer: IntegerProperty = -1
  var currentFilter: IntegerProperty = 0
}

abstract class ListableThing {
  var name: StringProperty
}

class Server extends ListableThing {
  var name: StringProperty = "<new>"
  var id: StringProperty = new java.util.Date().getTime.toString
  var localFolder: StringProperty = ""
  var filterRegexp: StringProperty = ""
  var protocols = new sfxc.ObservableBuffer[Protocol]
  var currentProtocol: IntegerProperty = -1
  var subfolders = new sfxc.ObservableBuffer[SubFolder]
  var currentSubFolder: IntegerProperty = -1
  var skipEqualFiles: BooleanProperty = false
  override def toString: String = name // used for listview
}

class Protocol extends ListableThing {
  var name: StringProperty = "<new>"
  var protocoluri: StringProperty = ""
  var protocolbasefolder: StringProperty = ""
  var executeBefore: StringProperty = ""
  var executeAfter: StringProperty = ""
  override def toString: String = name
}

class SubFolder extends ListableThing {
  var name: StringProperty = "<new>"
  var subfolder: StringProperty = ""
  override def toString: String = name
}


object Store {
  var config : Config = null

  def save() {
    println("-----------save " + config)
    val fff = Path.fromString(DBSettings.getSettingPath)
    def saveVal(key: String, what: Property[_,_]) = {
      fff.append(key + "," + what.value + "\n")
    }
    fff.write("sfsyncsettingsversion,1\n")
    saveVal("servercurr", config.currentServer)
    saveVal("currentFilter", config.currentFilter)
    for (server <- config.servers) {
      saveVal("server", server.name)
      saveVal("localfolder", server.localFolder)
      saveVal("filterregexp", server.filterRegexp)
      saveVal("id", server.id)
      saveVal("protocolcurr", server.currentProtocol)
      saveVal("skipequalfiles", server.skipEqualFiles)
      for (proto <- server.protocols) {
        saveVal("protocol", proto.name)
        saveVal("protocoluri", proto.protocoluri)
        saveVal("protocolbasefolder", proto.protocolbasefolder)
        saveVal("protocolexbefore", proto.executeBefore)
        saveVal("protocolexafter", proto.executeAfter)
      }
      saveVal("subfoldercurr", server.currentSubFolder)
      for (subf <- server.subfolders) {
        saveVal("subfolder", subf.name)
        saveVal("subfolderfolder", subf.subfolder)
      }
    }
    println("-----------/save")
  }

  def load() {
    var lastserver: Server = null
    var lastprotocol: Protocol = null
    var lastsubfolder: SubFolder = null
    println("----------load")
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
          }
          case "localfolder" => { lastserver.localFolder = sett(1) }
          case "filterregexp" => { lastserver.filterRegexp = sett(1) }
          case "id" => { lastserver.id = sett(1) }
          case "skipequalfiles" => {lastserver.skipEqualFiles = sett(1).toBoolean }
          case "protocolcurr" => { lastserver.currentProtocol = sett(1).toInt }
          case "protocol" => {
            lastprotocol = new Protocol { name = sett(1) }
            lastserver.protocols.add(lastprotocol)
          }
          case "protocoluri" => {lastprotocol.protocoluri = sett(1)}
          case "protocolbasefolder" => {lastprotocol.protocolbasefolder = sett(1)}
          case "protocolexbefore" => {lastprotocol.executeBefore = sett(1)}
          case "protocolexafter" => {lastprotocol.executeAfter = sett(1)}
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
//      println(" removed from cache " + vf)
    } else {
      println(" error: cache doesn't contain " + vf)
    }
  }

  def addupdate(vf: VirtualFile) {
    val vfs = cache.filter(p => p.path == vf.path)
//    println(" addup: found " + vfs)
    cache --= vfs
    cache += vf
    val vfs2 = cache.filter(p => p.path == vf.path)
//    println(" after: found " + vfs2)
  }

  def saveCache(name: String) {
    val fff = Path.fromString(getCacheFilename(name))
    if (fff.exists) {
      fff.delete(force = true)
    }
    fff.doCreateFile()
    for (cf <- cache) {
//      println("  savecache: " + cf)
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

object TestStoreMakeMany extends App {
  val basef = Path.fromString("/Unencrypted_Data/tempnospotlight/teststorelargelocal")
  basef.exists && sys.error("basef exists")
  basef.createDirectory()
  for(i <- 1 to 200) {
    val baseff = basef / ("folder" + i)
    baseff.createDirectory()
    for (j<- 1 to 100) {
      baseff / ("file" + i + "-" + j) doCreateFile()
    }
  }
  println("done")
}
