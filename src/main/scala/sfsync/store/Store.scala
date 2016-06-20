package sfsync.store

import sfsync.store.Tools._
import sfsync.util.Helpers._
import sfsync.CF
import sfsync.synchro.Actions._
import sfsync.util.Logging

import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer._
import scalafx.{collections => sfxc}
import scalafx.beans.property._
import scala.collection.mutable.ArrayBuffer

import scala.collection.JavaConversions._

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

  val lockFile = new java.io.File(settpath + "/sfsync.lock")

  def dbpath(name: String) = dbdir + "/" + name

  def getSettingPath = settpath + "/sfsyncsettings" + ".txt"

  def getLock = lockFile.createNewFile()

  def releaseLock() = lockFile.delete()

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
  var servers = new sfxc.ObservableBuffer[Server]
  var currentServer = IntegerProperty(-1)
  var currentFilter = IntegerProperty(0)
  var width = IntegerProperty(800)
  var height = IntegerProperty(600)
  var x = IntegerProperty (100)
  var y = IntegerProperty(100)
  var dividerPositions = new ArrayBuffer[Double]
  var cryptoSecret = ""
}

class ListableThing extends Ordered[ListableThing] {
  var name = StringProperty("<new>")
  def compare(that: ListableThing): Int = this.name.getValueSafe.compareTo(that.name.getValueSafe)
}

class MyList[T] extends ArrayBuffer[T] {
  def add(what: T) = { this += what}
}

class Server extends ListableThing {
  var id = StringProperty(new java.util.Date().getTime.toString)
  var cantSetDate = BooleanProperty(false)
  var localFolder = StringProperty("")
  var filterRegexp = StringProperty("")
  var protocols = new sfxc.ObservableBuffer[Protocol]
  var currentProtocol = IntegerProperty(-1)
  var subfolders = new sfxc.ObservableBuffer[SubFolder]
  var currentSubFolder = IntegerProperty(-1)
  override def toString: String = name.getValueSafe // used for listview
}

class Protocol extends ListableThing {
  var protocoluri = StringProperty("file:///")
  var protocolbasefolder = StringProperty("")
  var executeBefore = StringProperty("")
  var executeAfter = StringProperty("")
  override def toString: String = name.getValueSafe
}

class SubFolder extends ListableThing {
  var subfolders = new sfxc.ObservableBuffer[String]()
  override def toString: String = name.getValueSafe
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
      saveVal("cantsetdate", server.cantSetDate)
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
    if (lines.isEmpty) {
      info("no config file...")
      config = new Config
    } else {
      lines.foreach(lll => {
        val sett = splitsetting(lll.toString)
        sett.head match {
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
            lastserver = new Server { name.value = sett(1) }
            config.servers += lastserver
          case "localfolder" => lastserver.localFolder.value = sett(1)
          case "cantsetdate" => lastserver.cantSetDate.value = sett(1).toBoolean
          case "filterregexp" => lastserver.filterRegexp.value = sett(1)
          case "id" => lastserver.id.value = sett(1)
          case "protocolcurr" => lastserver.currentProtocol.value = sett(1).toInt
          case "protocol" =>
            lastprotocol = new Protocol { name.value = sett(1) }
            lastserver.protocols += lastprotocol
          case "protocoluri" => lastprotocol.protocoluri.value = sett(1)
          case "protocolbasefolder" => lastprotocol.protocolbasefolder.value = sett(1)
          case "protocolexbefore" => lastprotocol.executeBefore.value = sett(1)
          case "protocolexafter" => lastprotocol.executeAfter.value = sett(1)
          case "subfoldercurr" => lastserver.currentSubFolder.value = sett(1).toInt
          case "subfolder" =>
            lastsubfolder = new SubFolder { name.value = sett(1) }
            lastserver.subfolders += lastsubfolder
          case "subfolderfolder" => lastsubfolder.subfolders.add(sett(1))
          case _ => warn("unknown tag in config file: <" + sett.head + ">")
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
  override def toString = {s"[path=$path action=[${CF.amap(se.action)}] lTime=${se.lTime} lSize=${se.lSize} rTime=${se.rTime} rSize=${se.rSize} lcTime=${se.lcTime} rcTime=${se.rcTime} cSize=${se.cSize} rel=${se.relevant}"}
  def toStringNice = {
    s"""
     |Path: TODO
     |Local : ${se.detailsLocal.value}
     |Remote: ${se.detailsRemote.value}
     |LCache : ${se.detailsLCache.value}
     |RCache : ${se.detailsRCache.value} (${se.hasCachedParent})
    """.stripMargin
  }
}

// if path endswith '/', it's a dir!!!
// if ?Size == -1: file does not exist
class SyncEntry(var action: Int,
                var lTime: Long, var lSize: Long,
                var rTime: Long, var rSize: Long,
                var lcTime: Long, var rcTime: Long, var cSize: Long,
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
  def detailsRCache = new StringProperty(this, "detailsrc",
    if (cSize != -1) dformat.format(new java.util.Date(rcTime)) + "(" + cSize + ")" else "none")
  def detailsLCache = new StringProperty(this, "detailslc",
    if (cSize != -1) dformat.format(new java.util.Date(lcTime)) + "(" + cSize + ")" else "none")
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
      if (lSize != -1 && lSize == cSize && sameTime(lTime, lcTime)) true else false
    }
  }
  def isReqC = {
    if (isDir) {
      if (rSize != -1 && cSize != -1) true else false
    } else {
      if (rSize != -1 && rSize == cSize && sameTime(rTime, rcTime)) true else false
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
      if (isLeqC && isReqC ) action = A_ISEQUAL // apparently, cantSetDate=true
      else if ( isLeqC && rSize == -1) action = A_RMLOCAL // remote was deleted (local still in cache)
      else if (lSize == -1 && isReqC ) action = A_RMREMOTE // local was deleted (remote still in cache)
      // both exist, as does fcache
      else if ( isLeqC && rTime > rcTime) action = A_USEREMOTE // flocal unchanged, remote newer
      else if ( isReqC && lTime > lcTime) action = A_USELOCAL // fremote unchanged, local newer
      else action = A_UNKNOWN // both changed and all other strange things that might occur
    }
    assert(action != -9)
    this
  }

  override def toString = s"[action=[${CF.amap(action)}] lTime=$lTime lSize=$lSize rTime=$rTime rSize=$rSize lcTime=$lcTime rcTime=$rcTime cSize=$cSize rel=$relevant"

}

// MUST be sorted like treemap: first, cache file is put here, then possibly new files added, which must be sorted.
// need fast access by path (==key): hashmap. only solution: treemap.
// must be synchronized: better ConcurrentSkipListMap as synchronizedSortedMap(TreeMap) locks whole thing
class MyTreeMap[K, V] extends java.util.concurrent.ConcurrentSkipListMap[K, V] {
  // old with treemap: this is 10x faster than foreach, can do it.remove(), return false to stop (or true/Unit for continue)
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

//class Asdfds extends com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList {
//  override def get(i: Int): Nothing = ???
//
//  override def size(): Int = ???
//}
//class MyObsBuffer[T] extends sfxc.ObservableBuffer[T] {
//
//}

// this holds the main database of files. also takes care of GUI observable list
object Cache extends Logging {
  val CACHEVERSION = "V1"
  var cache: MyTreeMap[String, SyncEntry] = null
  var observableListSleep = false
  var observableList = new sfxc.ObservableBuffer[SyncEntry2]()
  observableList.onChange( // automatically update treemap from UI changes
    (source: ObservableBuffer[SyncEntry2], changes: Seq[Change]) => {
      if (!observableListSleep) changes.foreach {
        case Update(from, to) => observableList.subList(from, to).foreach(se2 => {
          debug("changed se2: " + se2.toStringNice)
          cache.update(se2.path, se2.se)
        })
        case _ => throw new NotImplementedError("obslist wants to do something else! ")
      }
    }
  )

  def dumpAll() = {
    cache.iterate( (it, path, se) => println(path + ": " + se.toString))
  }

  def getCacheFilename(name: String) = {
    "" + DBSettings.dbpath(name) + "-cache.txt"
  }

  def iniCache() = {
    cache = new MyTreeMap[String, SyncEntry]()
    observableListSleep = true
    observableList.clear()
    observableListSleep = false
  }

  def loadCache(name: String) = {
    info("load cache database..." + name)
    iniCache()

    val fff = Paths.get(getCacheFilename(name))
    if (!Files.exists(fff)) {
      info("create cache file!")
      if (!Files.exists(fff.getParent)) Files.createDirectories(fff.getParent)
      Files.createFile(fff)
    }
    val br = Files.newBufferedReader(fff, filecharset)
    val cacheVersion = br.readLine()
    if (cacheVersion == CACHEVERSION) {
      var lll: String = null
      while ({lll = br.readLine() ; lll != null}) {
        var sett = splitsetting(lll.toString)
        val lcTime = sett.head.toLong
        sett = splitsetting(sett(1))
        val rcTime = sett.head.toLong
        sett = splitsetting(sett(1))
        val size = sett.head.toLong
        val path = sett(1) // this is safe, also commas in filename ok
        val vf = new SyncEntry(A_UNKNOWN, -1, -1, -1, -1, lcTime, rcTime, size, path.endsWith("/"), false)
        cache.put(path, vf)
      }
    } else {
      info(s"Don't load cache, wrong cache version $cacheVersion <> $CACHEVERSION")
    }
    br.close()
    updateObservableBuffer()
    info("cache database loaded!")
  }

  def saveCache(name: String) {
    info("save cache database..." + name)
    val fff = new java.io.File(getCacheFilename(name)) // forget scalax.io.file: much too slow
    if (fff.exists) fff.delete()
    val out = new java.io.BufferedWriter(new java.io.FileWriter(fff),1000000)
    out.write(CACHEVERSION + "\n")
    for ((path, cf: SyncEntry) <- cache) {
      out.write("" + cf.lcTime + "," + cf.rcTime + "," + cf.cSize + "," + path + "\n")
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
    cache.clear()
    observableList.clear()
  }

  // for listview
  var filterActions: List[Int] = ALLACTIONS

  def updateObservableBuffer() {
    debug("update obs buffer...")
    observableListSleep = true
    observableList.clear()

    // fill obslist
    cache.iterate( (it, path, se) => {
      if (se.relevant && filterActions.contains(se.action)) {
        observableList += new SyncEntry2(path, se)
      } else true
    })
    observableListSleep = false
    debug("update obs buffer done!")
  }

  def canSync: Boolean = {
    for ( (path, se:SyncEntry) <- cache) {
      if (se.relevant && se.action == A_UNKNOWN) return false
    }
    true
  }



}
