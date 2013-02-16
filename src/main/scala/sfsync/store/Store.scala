package sfsync.store

import sfsync.synchro.VirtualFile
import scalafx.{collections => sfxc}
import scalafx.beans.property._
import scalax.file.Path
import scalax.io.Line
import Tools._
import sfsync.Helpers._
import collection.mutable.ArrayBuffer
import scala.language.implicitConversions


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
  val crypto = new JavaCryptoEncryption("DES")
}

class JavaCryptoEncryption(algorithmName: String) {
  import javax.crypto.spec.SecretKeySpec
  import javax.crypto.Cipher
  val b64enc = new sun.misc.BASE64Encoder()
  val b64dec = new sun.misc.BASE64Decoder()
  def encrypt(bytes: String, secret: String): String = {
    val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
    val encipher = Cipher.getInstance(algorithmName + "/ECB/PKCS5Padding")
    encipher.init(Cipher.ENCRYPT_MODE, secretKey)
    val res = encipher.doFinal(bytes.getBytes("UTF-8"))
    b64enc.encode(res).replaceAll("/", "-")
  }

  def decrypt(bytes: String, secret: String): String = {
    val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
    val encipher = Cipher.getInstance(algorithmName + "/ECB/PKCS5Padding")
    encipher.init(Cipher.DECRYPT_MODE, secretKey)
    val bytes2 = bytes.replaceAll("-","/")
    val res = encipher.doFinal(b64dec.decodeBuffer(bytes2))
    new String(res, "UTF-8")
  }
}

class Config {
  // implicit def StringToStringProperty(s: String): StringProperty = StringProperty(s)
  implicit def IntegerToIntegerProperty(i: Int): IntegerProperty = IntegerProperty(i)
  var servers = new sfxc.ObservableBuffer[Server]
  var currentServer: IntegerProperty = -1
  var currentFilter: IntegerProperty = 0
  var width: IntegerProperty = 800
  var height: IntegerProperty = 600
  var dividerPositions = new ArrayBuffer[Double]
}

class ListableThing extends Ordered[ListableThing] {
  var name: StringProperty = StringProperty("<new>")
  def compare(that: ListableThing): Int = this.name.compareTo(that.name)
}

class MyList[T] extends ArrayBuffer[T] {
  def add(what: T) = { this += what}
}

class Server extends ListableThing {
  implicit def StringToStringProperty(s: String): StringProperty = StringProperty(s)
  implicit def IntegerToIntegerProperty(i: Int): IntegerProperty = IntegerProperty(i)
  var id: StringProperty = new java.util.Date().getTime.toString
  var localFolder: StringProperty = ""
  var filterRegexp: StringProperty = ""
  var protocols = new sfxc.ObservableBuffer[Protocol]
  var currentProtocol: IntegerProperty = -1
  var subfolders = new sfxc.ObservableBuffer[SubFolder]
  var currentSubFolder: IntegerProperty = -1
  var skipEqualFiles: BooleanProperty = BooleanProperty(value = false)
  override def toString: String = name // used for listview
}

class Protocol extends ListableThing {
  implicit def StringToStringProperty(s: String): StringProperty = StringProperty(s)
  implicit def IntegerToIntegerProperty(i: Int): IntegerProperty = IntegerProperty(i)
  var protocoluri: StringProperty = ""
  var protocolbasefolder: StringProperty = ""
  var executeBefore: StringProperty = ""
  var executeAfter: StringProperty = ""
  override def toString: String = name
}

class SubFolder extends ListableThing {
  implicit def StringToStringProperty(s: String): StringProperty = StringProperty(s)
  implicit def IntegerToIntegerProperty(i: Int): IntegerProperty = IntegerProperty(i)
  var subfolders = new sfxc.ObservableBuffer[String]()
  override def toString: String = name
}


object Store {
  var config : Config = null

  def save() {
    println("-----------save " + config)
    val fff = Path.fromString(DBSettings.getSettingPath)
    def saveVal(key: String, what: Property[_,_]) {
      fff.append(key + "," + what.value + "\n")
    }
    def saveString(key: String, what: String) {
      fff.append(key + "," + what + "\n")
    }
    fff.write("sfsyncsettingsversion,1\n")
    saveVal("width", config.width)
    saveVal("height", config.height)
    saveString("dividerpositions", config.dividerPositions.mkString("#"))
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
        for (subff <- subf.subfolders) {
          saveString("subfolderfolder", subff)
        }
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
          case "width" => { config.width.value = sett(1).toInt }
          case "height" => { config.height.value = sett(1).toInt }
          case "dividerpositions" => { config.dividerPositions ++= (sett(1).split("#").map(x => x.toDouble))}
          case "servercurr" => { config.currentServer.value = sett(1).toInt }
          case "currentFilter" => config.currentFilter.value = sett(1).toInt
          case "server" => {
            lastserver = new Server { name = sett(1) }
            config.servers += lastserver
          }
          case "localfolder" => { lastserver.localFolder.value = sett(1) }
          case "filterregexp" => { lastserver.filterRegexp.value = sett(1) }
          case "id" => { lastserver.id.value = sett(1) }
          case "skipequalfiles" => {lastserver.skipEqualFiles.value = sett(1).toBoolean }
          case "protocolcurr" => { lastserver.currentProtocol.value = sett(1).toInt }
          case "protocol" => {
            lastprotocol = new Protocol { name = sett(1) }
            lastserver.protocols += lastprotocol
          }
          case "protocoluri" => {lastprotocol.protocoluri.value = sett(1)}
          case "protocolbasefolder" => {lastprotocol.protocolbasefolder.value = sett(1)}
          case "protocolexbefore" => {lastprotocol.executeBefore.value = sett(1)}
          case "protocolexafter" => {lastprotocol.executeAfter.value = sett(1)}
          case "subfoldercurr" => { lastserver.currentSubFolder.value = sett(1).toInt }
          case "subfolder" => {
            lastsubfolder = new SubFolder { name = sett(1) }
            lastserver.subfolders += lastsubfolder
          }
          case "subfolderfolder" => {lastsubfolder.subfolders.add(sett(1))}
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
    } else {
      println(" error: cache doesn't contain " + vf)
    }
  }

  def addupdate(vf: VirtualFile) {
    val vfs = cache.filter(p => p.path == vf.path)
    cache --= vfs
    cache += vf
  }

  def saveCache(name: String) {
    val fff = new java.io.File(name)
    if (fff.exists) fff.delete()
    val out = new java.io.BufferedWriter(new java.io.FileWriter(fff))

    for (cf <- cache) {
      out.write("" + cf.modTime + "," + cf.isDir + "," + cf.size + "," + cf.path + "\n")
    }
    out.close
    println("***** cache saved!")
    // forget scalax.io.file: much too slow
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
