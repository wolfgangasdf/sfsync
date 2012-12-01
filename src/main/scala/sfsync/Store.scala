package sfsync.store

import sfsync.synchro.VirtualFile
import scalafx.{collections => sfxc}
import collection.mutable
import scalafx.beans.property.StringProperty
import scalax.file.Path
import scalax.io.Line


object DBSettings {
  def dbpath = "/tmp/sfsyncsettings.txt"
  def getLines = {
    val fff = Path.fromString(DBSettings.dbpath)
    if (!fff.exists) {
      fff.doCreateFile()
    }
    fff.lines(Line.Terminators.NewLine,true)
  }
}


class Config {
  var servers = new sfxc.ObservableBuffer[Server] // THIS does not work :-( if the servers is used by ListView, it crashes the DB.....
//  var servers = new mutable.ArrayBuffer[Server] //
  var currentServer = -1;
}

class Server {
  var name: String = "<new>"
  var localFolder: String = ""
  var protocols = new sfxc.ObservableBuffer[Protocol]
  var currentProtocol = -1;
  var subfolders = new sfxc.ObservableBuffer[SubFolder]
  var currentSubFolder = -1;
//  var cache = new mutable.ArrayBuffer[VirtualFile]

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

/*
things are just added to last server!
sfsyncsettingsversion,1
servercurr,2
server,servname1
protocolcurr,2
protocol,protname1
protocol,protname2
subdir,subdir1
subdir,subdir2
server,servname2
protocolcurr,1
protocol,protname1
subdir,subdir1
 */

object Store {
  var config : Config = null

  def save {
    println("-----------save " + config)
    val fff = Path.fromString(DBSettings.dbpath)
    fff.write("sfsyncsettingsversion,1\n")
    fff.append("servercurr," + config.currentServer + "\n")
    for (server <- config.servers) {
      fff.append("server," + server.name + "\n")
      fff.append("localfolder," + server.localFolder + "\n")
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

  def load {
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
          case "server" => {
            lastserver = new Server { name = sett(1) }
            config.servers.add(lastserver)
          }
          case "localfolder" => { lastserver.localFolder = sett(1) }
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

  def dumpConfig {
    println("--------------dumpconfig")
    for (server <- config.servers) {
      println("server: " + server)
//      server.protocols.foreach( proto => println(proto))
//      server.subfolders.foreach( sf => println(sf) )
    }
    println("--------------/dumpconfig")
  }

  def splitsetting(ss: String) : List[String] = {
    val commapos = ss.indexOf(",")
    val tag = ss.substring(0,commapos)
    val content = ss.substring(commapos+1).trim
//    println(tag+" , " + content)
    List(tag,content)
  }

  // Load config
  load



}

