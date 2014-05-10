package sfsync.store

import scalafx.{collections => sfxc}
import scalafx.beans.property._
import Tools._
import sfsync.Helpers._
import collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import java.nio.file._
import org.squeryl.adapters.H2Adapter
import org.squeryl.PrimitiveTypeMode._
import sfsync.CF
import collection.mutable
import scala.Some
import org.squeryl.{Schema, Optimistic, KeyedEntity, Session, SessionFactory}
import sfsync.synchro.Actions
import sfsync.util.Logging
import Actions._
import scala.language.{reflectiveCalls, postfixOps}


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
    if (config.cryptoSecret == "") { // make sure we have a secret...
      val random = new scala.util.Random(new java.security.SecureRandom())
      config.cryptoSecret = random.alphanumeric.take(10).mkString
    }
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

class BaseEntity extends KeyedEntity[Long] {
  var id: Long = 0
}

// if path endswith '/', it's a dir!!!
// if ?Size == -1: file does not exist
class SyncEntry(var path: String, var action: Int,
                var lTime: Long, var lSize: Long,
                var rTime: Long, var rSize: Long,
                var cTime: Long, var cSize: Long,
                var relevant: Boolean,
                var selected: Boolean = false,
                var delete: Boolean = false
                 ) extends BaseEntity with Optimistic {
  var hasCachedParent = false // only used for folders!
  def sameTime(t1: Long, t2: Long) = Math.abs(t1 - t2) < 2000 // in milliseconds

  def status = new StringProperty(this, "status", CF.amap(action))
  def dformat = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
  def detailsLocal = new StringProperty(this, "detailsl",
      if (lSize != -1) dformat.format(new java.util.Date(lTime)) + "," + lSize else "none")
  def detailsRemote = new StringProperty(this, "detailsr",
    if (rSize != -1) dformat.format(new java.util.Date(rTime)) + "," + rSize else "none")
  def detailsCache = new StringProperty(this, "detailsc",
    if (cSize != -1) dformat.format(new java.util.Date(cTime)) + "," + cSize else "none")
  def isDir = path.endsWith("/")
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
  override def toString = {s"[path=$path action=$action lTime=$lTime lSize=$lSize rTime=$rTime rSize=$rSize cTime=$cTime cSize=$cSize rel=$relevant"}
  def toStringNice = {
    s"""
     |Path: $path
     |Local : ${detailsLocal.value}
     |Remote: ${detailsRemote.value}
     |Cache : ${detailsCache.value} (${hasCachedParent})
    """.stripMargin
  }
}

object MySchema extends Schema {
  val files = table[SyncEntry]

  on(files)(file => declare(
    file.id is (primaryKey,autoIncremented),
    file.path is (indexed, unique, dbType("varchar(16384)"))
  ))
}

object CacheDB extends Logging {

  val sessionFactory = SessionFactory
  var connected = false
  var sizeCache: Int = -1
  var seCache = new mutable.HashMap[Int,SyncEntry]()
  var syncEntries: com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[SyncEntry] = null
  var seSession: Session = null

  def getSession: Session = {
    if (connected) {
      val session = sessionFactory.newSession
      debug("getSession: new session " + session + " in thread " + Thread.currentThread().getId)
      session
    } else null
  }
  def getSESession = {
    if (connected) {
      if (seSession == null) {
        seSession = getSession
        debug("getSESession: new SEsession " + seSession + " in thread " + Thread.currentThread().getId)
      }
    }
    seSession
  }
  def updateSE(se: SyncEntry, clearCache: Boolean = true) {
    using(getSESession) {
      MySchema.files.update(se)
      if (clearCache) invalidateCache()
    }
  }
  def closeSEsession() {
    if (seSession != null) {
      seSession.close
      seSession = null
    }
  }
  def invalidateCache() {
    sizeCache = -1
    seCache.clear()
    closeSEsession()
  }

  // implement caching DB provider
  def initializeSyncEntries(onlyRelevant: Option[Boolean], filterActions: List[Int] = ALLACTIONS) {
    closeSEsession()
    syncEntries =  new com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[SyncEntry]() {
      def get(p1: Int): SyncEntry = {
        try {
          if (!seCache.contains(p1)) {
            if (seCache.size > 5000) seCache.clear() // TODO increase massively?
            debug("get p1=" + p1)
            using(getSESession) {
              var startindex = p1 - 10 // cache for scrolling etc // TODO more?
              if (startindex < 0 ) startindex = 0
              val res =
                from(MySchema.files) (se =>
                  where(
                    (se.relevant === onlyRelevant.?) and
                    (se.action in filterActions)
                  )
                  select se
                  orderBy se.path
                ) page(startindex,300)
              var iii = startindex
              res.foreach(se => { // update cache
                if (!seCache.contains(iii)) seCache.put(iii,se)
                iii += 1
              })
            }
          }
        } catch { case e: Exception => warn("se.get: ignored exception:" + e) }
        seCache.get(p1).getOrElse(null)
      }
      def size(): Int = {
        try {
          if (sizeCache == -1) {
            if (getSession != null)
              using(getSESession) {
                sizeCache =
                  from(MySchema.files)(se =>
                    where(
                      (se.relevant === onlyRelevant.?) and
                        (se.action in filterActions)
                    )
                      select se
                  ).size
                debug("get size=" + sizeCache)
              }
            else sizeCache = 0
          }
        } catch { case e: Exception => warn("se.size: ignored exception:" + e) }
        sizeCache
      }
// TODO: needed?
//      override def toArray[T](a: Array[T]): Array[T] = { // disabled...
//        throw new UnsupportedOperationException("internal error toarray")
//        null
//      }
    }
  }

  def cleanup() {
    if (seSession != null) {
      seSession.close
      seSession = null
    }
    invalidateCache()
  }

  def canSync = {
    transaction {
      val si = MySchema.files.where(se => (se.relevant === true) and (se.action === A_UNKNOWN)).size
      si == 0
    }
  }

  def connectDB(name: String) = {
    debug("connectDB() in thread " + Thread.currentThread().getId)
    cleanup()
    connected = false

    info("connecting to database name=" + name + " at " + DBSettings.dbpath(name))
    Class.forName("org.h2.Driver")
    val dbexists = Files.exists(Paths.get(DBSettings.dbpath(name) + ".h2.db"))
    val databaseConnection = s"jdbc:h2:" + DBSettings.dbpath(name) + ";MVCC=TRUE;CACHE_SIZE=131072" + (if (dbexists) ";create=true" else "")
    sessionFactory.concreteFactory = Some(() => {
      Session.create(java.sql.DriverManager.getConnection(databaseConnection), new H2Adapter)
    })

    connected = true
    transaction {
      if (!dbexists) {
        MySchema.create
        info("  Created the schema")
        MySchema.printDdl
      }
    }
    initializeSyncEntries(onlyRelevant = Option(false))
    dbexists
  }

  def clearDB() {
    transaction {
      MySchema.drop // now drop database to make updates easier!
      MySchema.create
      info("  Created the schema")
      MySchema.printDdl
//      MySchema.files.deleteWhere(se => se.id.isNotNull)
      invalidateCache()
    }
  }

}
