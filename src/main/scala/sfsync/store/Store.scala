package sfsync.store

import sfsync.store.Tools._
import sfsync.Helpers._
import sfsync.CF
import sfsync.synchro.Actions._
import sfsync.util.Logging

import scalafx.{collections => sfxc}
import scalafx.beans.property._
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.language.{reflectiveCalls, postfixOps}

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

import java.nio.file._


object DBSettings extends Logging {
  var settpath = ""
  var dbdir = ""
  if (isMac) {
    settpath = System.getProperty("user.home") + "/Library/Application Support/SFSync"
    dbdir = System.getProperty("user.home") + "/Library/Caches/SFSync"
  } else if (isLinux) {
    settpath = System.getProperty("user.home") + "/.sfsync"
    dbdir = settpath + "/cache"
  } else if (isWin) {
    settpath = toJavaPathSeparator(System.getenv("APPDATA")) + "/SFSync"
    dbdir = settpath + "/cache"
  } else throw new Exception("operating system not found")

  def dbpath(name: String) = dbdir + "/" + name

  def getSettingPath = settpath + "/sfsyncsettings" + ".txt"

  def getLines = {
    val fff = Paths.get(getSettingPath)
    if (!Files.exists(fff)) {
      info("creating setting file " + fff.toString)
      Files.createDirectories(fff.getParent)
      Files.createFile(fff)
    }
    Files.readAllLines(fff, filecharset).toArray
  }
}

object Tools {
  // splits safely: at first comma
  def splitsetting(ss: String) : List[String] = {
    val commapos = ss.indexOf(",")
    val tag = ss.substring(0,commapos)
    val content = ss.substring(commapos+1).trim
    //    debug(tag+" , " + content)
    List(tag,content)
  }
  val crypto = new JavaCryptoEncryption("DES")
}

class JavaCryptoEncryption(algorithmName: String) {
  import javax.crypto.spec.SecretKeySpec
  import javax.crypto.Cipher
  val b64enc = new sun.misc.BASE64Encoder()
  val b64dec = new sun.misc.BASE64Decoder()
  def getNewSecret = {
    val random = new scala.util.Random(new java.security.SecureRandom())
    random.alphanumeric.take(8).mkString
  }
  def encrypt(bytes: String): String = {
    val secretKey = new SecretKeySpec(Store.config.cryptoSecret.getBytes("UTF-8"), algorithmName)
    val encipher = Cipher.getInstance(algorithmName + "/ECB/PKCS5Padding")
    encipher.init(Cipher.ENCRYPT_MODE, secretKey)
    val res = encipher.doFinal(bytes.getBytes("UTF-8"))
    b64enc.encode(res).replaceAll("/", "-")
  }

  def decrypt(bytes: String): String = {
    val secretKey = new SecretKeySpec(Store.config.cryptoSecret.getBytes("UTF-8"), algorithmName)
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
  var x: IntegerProperty = 100
  var y: IntegerProperty = 100
  var dividerPositions = new ArrayBuffer[Double]
  var cryptoSecret = ""
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
  override def toString: String = name // used for listview
}

class Protocol extends ListableThing {
  implicit def StringToStringProperty(s: String): StringProperty = StringProperty(s)
  implicit def IntegerToIntegerProperty(i: Int): IntegerProperty = IntegerProperty(i)
  var protocoluri: StringProperty = "file:///"
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


object Store extends Logging {
  var config : Config = null

  def save() {
    info("-----------save " + config)
    val fff = Paths.get(DBSettings.getSettingPath)
    Files.delete(fff)
    Files.createFile(fff)
    def saveVal(key: String, what: Property[_,_]) {
      Files.write(fff, (key + "," + what.value + "\n").getBytes(filecharset),StandardOpenOption.APPEND)
    }
    def saveString(key: String, what: String) {
      Files.write(fff, (key + "," + what + "\n").getBytes(filecharset),StandardOpenOption.APPEND)
    }
    Files.write(fff, "sfsyncsettingsversion,1\n".getBytes, StandardOpenOption.APPEND)
    saveVal("width", config.width)
    saveVal("height", config.height)
    saveString("dividerpositions", config.dividerPositions.mkString("#"))
    saveVal("servercurr", config.currentServer)
    saveVal("currentFilter", config.currentFilter)
    saveString("cryptoSecret", config.cryptoSecret)
    for (server <- config.servers) {
      saveVal("server", server.name)
      saveVal("localfolder", server.localFolder)
      saveVal("filterregexp", server.filterRegexp)
      saveVal("id", server.id)
      saveVal("protocolcurr", server.currentProtocol)
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
    info("-----------/save")
  }

  def load() {
    var lastserver: Server = null
    var lastprotocol: Protocol = null
    var lastsubfolder: SubFolder = null
    info("----------load")
    val lines = DBSettings.getLines
    if (lines.size == 0) {
      info("no config file...")
      config = new Config
    } else {
      lines.foreach(lll => {
        val sett = splitsetting(lll.toString)
        sett(0) match {
          case "sfsyncsettingsversion" =>
            if (!sett(1).equals("1")) sys.error("wrong settings version")
            config = new Config()
          case "width" => config.width.value = sett(1).toInt
          case "height" => config.height.value = sett(1).toInt
          case "dividerpositions" => config.dividerPositions ++= sett(1).split("#").map(x => x.toDouble)
          case "servercurr" => config.currentServer.value = sett(1).toInt
          case "currentFilter" => config.currentFilter.value = sett(1).toInt
          case "cryptoSecret" => config.cryptoSecret = sett(1)
          case "server" =>
            lastserver = new Server { name = sett(1) }
            config.servers += lastserver
          case "localfolder" => lastserver.localFolder.value = sett(1)
          case "filterregexp" => lastserver.filterRegexp.value = sett(1)
          case "id" => lastserver.id.value = sett(1)
          case "protocolcurr" => lastserver.currentProtocol.value = sett(1).toInt
          case "protocol" =>
            lastprotocol = new Protocol { name = sett(1) }
            lastserver.protocols += lastprotocol
          case "protocoluri" => lastprotocol.protocoluri.value = sett(1)
          case "protocolbasefolder" => lastprotocol.protocolbasefolder.value = sett(1)
          case "protocolexbefore" => lastprotocol.executeBefore.value = sett(1)
          case "protocolexafter" => lastprotocol.executeAfter.value = sett(1)
          case "subfoldercurr" => lastserver.currentSubFolder.value = sett(1).toInt
          case "subfolder" =>
            lastsubfolder = new SubFolder { name = sett(1) }
            lastserver.subfolders += lastsubfolder
          case "subfolderfolder" => lastsubfolder.subfolders.add(sett(1))
          case _ => warn("unknown tag in config file: <" + sett(0) + ">")
        }
      })
    }
    if (config.cryptoSecret == "") config.cryptoSecret = Tools.crypto.getNewSecret // make sure we have a secret...
  }

  def dumpConfig() {
    info("--------------dumpconfig")
    for (server <- config.servers) {
      info("server: " + server + " currprot=" + server.currentProtocol + " currsf=" + server.currentSubFolder)
      server.protocols.foreach( proto => info("  proto: " + proto))
      server.subfolders.foreach( sf => info("  subfolder: " + sf) )
    }
    info("--------------/dumpconfig")
  }

  load()

}

class SyncEntry2(var path: String, var se: SyncEntry) {
  override def toString = {s"[path=TODO action=${se.action} lTime=${se.lTime} lSize=${se.lSize} rTime=${se.rTime} rSize=${se.rSize} cTime=${se.cTime} cSize=${se.cSize} rel=${se.relevant}"}
  def toStringNice = {
    s"""
     |Path: TODO
     |Local : ${se.detailsLocal.value}
     |Remote: ${se.detailsRemote.value}
     |Cache : ${se.detailsCache.value} (${se.hasCachedParent})
    """.stripMargin
  }
}

// if path endswith '/', it's a dir!!!
// if ?Size == -1: file does not exist
class SyncEntry(var action: Int,
                var lTime: Long, var lSize: Long,
                var rTime: Long, var rSize: Long,
                var cTime: Long, var cSize: Long,
                var isDir: Boolean,
                var relevant: Boolean,
                var selected: Boolean = false,
                var delete: Boolean = false
                 ) {
  var hasCachedParent = false // only used for folders!
  def sameTime(t1: Long, t2: Long) = Math.abs(t1 - t2) < 2000 // in milliseconds

  def status = new StringProperty(this, "status", CF.amap(action))
  def dformat = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
  def detailsLocal = new StringProperty(this, "detailsl",
      if (lSize != -1) dformat.format(new java.util.Date(lTime)) + "(" + lSize + ")" else "none")
  def detailsRemote = new StringProperty(this, "detailsr",
    if (rSize != -1) dformat.format(new java.util.Date(rTime)) + "(" + rSize + ")" else "none")
  def detailsCache = new StringProperty(this, "detailsc",
    if (cSize != -1) dformat.format(new java.util.Date(cTime)) + "(" + cSize + ")" else "none")
//  def isDir = path.endsWith("/")
  def isEqual = {
    if (isDir) {
      if (lSize != -1 && rSize != -1) true else false
    } else {
      if (lSize != -1 && lSize == rSize && sameTime(lTime, rTime)) true else false
    }
  }
  def isLeqC = {
    if (isDir) {
      if (lSize != -1 && cSize != -1) true else false
    } else {
      if (lSize != -1 && lSize == cSize && sameTime(lTime, cTime)) true else false
    }
  }
  def isReqC = {
    if (isDir) {
      if (rSize != -1 && cSize != -1) true else false
    } else {
      if (rSize != -1 && rSize == cSize && sameTime(rTime, cTime)) true else false
    }
  }

  def compareSetAction(newcache: Boolean) = {
    import sfsync.synchro.Actions._
    action = -9
    if (lSize == -1 && rSize == -1) { // cache only?
      action = A_CACHEONLY
    } else  if (isEqual) { // just equal?
      action = A_ISEQUAL
    } else if (cSize == -1) { // not in remote cache
      if (newcache) { // not equal, not in cache because cache new
        action = A_UNKNOWN
      } else { // not in cache but cache not new: new file?
        if (lSize != -1 && rSize == -1) action = A_USELOCAL // new local (cache not new)
        else if (lSize == -1 && rSize != -1) action = A_USEREMOTE // new remote (cache not new)
        else action = A_UNKNOWN // not in cache but both present
      }
    } else { // in cache, not equal
      if ( isLeqC && rSize == -1) action = A_RMLOCAL // remote was deleted (local still in cache)
      else if (lSize == -1 && isReqC ) action = A_RMREMOTE // local was deleted (remote still in cache)
      // both exist, as does fcache
      else if ( isLeqC && rTime > lTime) action = A_USEREMOTE // flocal unchanged, remote newer
      else if ( isReqC && lTime > rTime) action = A_USELOCAL // fremote unchanged, local newer
      else action = A_UNKNOWN // both changed and all other strange things that might occur
    }
    //  debug("CF: " + toString)
    assert(action != -9)
    //debug("iniaction: " + this.toString)
    this
  }

  override def toString = s"[action=$action lTime=$lTime lSize=$lSize rTime=$rTime rSize=$rSize cTime=$cTime cSize=$cSize rel=$relevant"

}


class MyTreeMap[K, V] extends java.util.TreeMap[K, V] {
  // this is 10x faster than foreach, can do it.remove(), return false to stop (or true/Unit for continue)
  def iterate(fun: ( java.util.Iterator[java.util.Map.Entry[K, V]], K, V ) => Any, reversed: Boolean = false) = {
    val it = if (reversed)
      this.descendingMap().entrySet().iterator()
    else
      this.entrySet().iterator()
    var fres = true
    while (it.hasNext && fres) {
      val ele = it.next()
      val fres1 = fun(it, ele.getKey, ele.getValue)
      fres = fres1 match {
        case fres2: Boolean => fres2
        case _ => true
      }
    }
  }
}


object Cache extends Logging {
  var cache: MyTreeMap[String, SyncEntry] = null
  var paginationcache = new MyTreeMap[Int, SyncEntry2]() // only for list view!
  private var cachemodified = false // make sure to reload listview if things modified!
  var observableList: com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[SyncEntry2] = null

  def dumpAll() = {
    cache.iterate( (it, path, se) => println(path + ": " + se.toString))
  }

  def getCacheFilename(name: String) = {
    "" + DBSettings.dbpath(name) + "-cache.txt"
  }

  def iniCache() = {
    cache = new MyTreeMap[String, SyncEntry]()
    cachemodified = true
  }

  def loadCache(name: String) = {
    info("load cache database..." + name)
    cache = new MyTreeMap[String, SyncEntry]()
    val fff = Paths.get(getCacheFilename(name))
    if (!Files.exists(fff)) {
      println("create cache file!")
      Files.createFile(fff)
    }
    val lines = Files.readAllLines(fff, filecharset).toArray
    lines.foreach(lll => {
      var sett = splitsetting(lll.toString)
      val modTime = sett(0).toLong
      sett = splitsetting(sett(1))
      val size = sett(0).toLong
      val path = sett(1) // this is safe, also commas in filename ok
      val vf = new SyncEntry(A_UNKNOWN, -1, -1, -1, -1, modTime, size, path.endsWith("/"), false)
      cache.put(path, vf)
    })
    info("cache database loaded!")
    cachemodified = true
  }

  def saveCache(name: String) {
    info("save cache database..." + name)
    val fff = new java.io.File(getCacheFilename(name)) // forget scalax.io.file: much too slow
    if (fff.exists) fff.delete()
    val out = new java.io.BufferedWriter(new java.io.FileWriter(fff),1000000)
    for ((path, cf: SyncEntry) <- cache) {
      out.write("" + cf.cTime + "," + cf.cSize + "," + path + "\n")
    }
    out.close()
    info("cache database saved!")
  }

  def clearCacheFile(name: String) {
    info("delete cache database " + name)
    val fff = Paths.get(getCacheFilename(name))
    if (Files.exists(fff)) {
      Files.delete(fff)
    }
  }
  
  // use this for any modification or set cachemodified manually!
  def update(key: String, se: SyncEntry) = {
    cache.put(key, se)
    cachemodified = true
  }

  def clearPaginationCache() = {
    paginationcache.clear()
    cachemodified = false
  }

  // for listview
  def initializeSyncEntries(onlyRelevant: Boolean, filterActions: List[Int] = ALLACTIONS) {
    observableList =  new com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[SyncEntry2]() {
      def get(p1: Int): SyncEntry2 = {
        if (!paginationcache.containsKey(p1) || cachemodified) {
          if (paginationcache.size > 5000 || cachemodified) {
            clearPaginationCache()
          }
          var startindex = p1 - 10 // cache for scrolling etc // TODO more?
          if (startindex < 0 ) startindex = 0
          debug("refresh cache p1=" + p1 + " startindex=" + startindex)
          var iii = 0
          cache.iterate( (it, path, se) => {
            if ((!onlyRelevant || se.relevant) && filterActions.contains(se.action)) {
              if (iii >= startindex)
                paginationcache.put(iii, new SyncEntry2(path, se))
              iii += 1
              if (iii > (startindex + 20)) false else true
            } else true
          })
        }
        paginationcache.get(p1)
      }
      def size(): Int = {
        var iii = 0
        cache.iterate((it, path, se) => {
          if ((!onlyRelevant || se.relevant) && filterActions.contains(se.action)) iii += 1
        })
        iii
      }
    }
  }

  def canSync: Boolean = {
    for ( (path, se:SyncEntry) <- cache) {
      if (se.relevant && se.action == A_UNKNOWN) return false
    }
    true
  }



}
