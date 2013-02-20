package sfsync.store

import sfsync.synchro.{ComparedFile, VirtualFile}
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
import javafx.collections.ListChangeListener.Change

object DBSettings {
  def settpath = {
    var res = ""
    if (isMac) res = System.getProperty("user.home") + "/Library/SFSync"
    else if (isLinux) res = System.getProperty("user.home") + "/.sfsync"
    else if (isWin) res = toJavaPathSeparator(System.getenv("APPDATA")) + "/SFSync"
    else throw new Exception("operating system not found")
    res
  }

  def settdbpath = settpath + "/sfsyncsettings"
  def getSettingPath = DBSettings.settdbpath + ".txt"
  def getLines = {
    val fff = Paths.get(getSettingPath)
    if (!Files.exists(fff)) {
      println("creating setting file " + fff.toString)
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

class BaseEntity extends KeyedEntity[Long] {
  var id: Long = 0
}

// if path endswith '/', it's a dir!!!
// if ?Size == -1: file does not exist
class SyncEntry(var path: String, var action: Int,
                var lTime: Long, var lSize: Long,
                var rTime: Long, var rSize: Long,
                var cTime: Long, var cSize: Long,
                var relevant: Boolean
                 ) extends BaseEntity with Optimistic {
  def status = new StringProperty(this, "status", CF.amap(action))
  def dformat = new java.text.SimpleDateFormat("dd-MM-yyyy HH:mm:ss")
  def detailsLocal = new StringProperty(this, "detailsl",
      (if (lSize != -1) (dformat.format(new java.util.Date(lTime)) + "," + lSize) else "none"))
  def detailsRemote = new StringProperty(this, "detailsr",
    (if (rSize != -1) (dformat.format(new java.util.Date(rTime)) + "," + rSize) else "none"))
  def changeAction() {
    // TODO
//        status.set(CF.amap(cf.action))
      }
  def iniAction(newcache: Boolean) = {
    import sfsync.synchro.Actions._
    action = -9
    if (lSize == -1 && rSize == -1 && cSize != -1) { // cache only?
      action = A_CACHEONLY
    } else  if (lSize == rSize && lTime == rTime) { // just equal?
      action = A_NOTHING
    } else if (cSize == -1) { // not in remote cache
      if (newcache)
        action = A_UNKNOWN // not equal and not in cache. unknown!
      else {
        if (lSize != -1 && rSize == -1) action = A_USELOCAL // new local (cache not new)
        else if (lSize == -1 && rSize != -1) action = A_USEREMOTE // new remote (cache not new)
        else action = A_UNKNOWN // not in cache but both present
      }
    } else { // in cache, newcache impossible
      if ( (lSize == cSize && lTime == cTime) && rSize == -1) action = A_RMLOCAL // remote was deleted (local still in cache)
      else if (lSize == -1 && (rSize == cSize && rTime == cTime) ) action = A_RMREMOTE // local was deleted (remote still in cache)
      // both exist, as does fcache
      else if ( (lSize == cSize && lTime == cTime) && rTime > lTime) action = A_USEREMOTE // flocal unchanged, remote newer
      else if ( (rSize == cSize && rTime == cTime) && lTime > rTime) action = A_USELOCAL // fremote unchanged, local newer
      else action = A_UNKNOWN // both changed and all other strange things that might occur
    }
    //  println("CF: " + toString)
    assert(action != -9)
    //println("iniaction: " + this.toString)
    this
  }
  override def toString = {s"[path=$path action=$action lTime=$lTime lSize=$lSize rTime=$rTime rSize=$rSize cTime=$cTime cSize=$cSize rel=$relevant"}
}

object MySchema extends Schema {
  val files = table[SyncEntry]

  on(files)(file => declare(
    file.id is (primaryKey,autoIncremented),
    file.path is (unique)
  ))
}

object CacheDB {

  var connected = false

  def getSession: Session = {
    if (connected) {
      val session = sessionFactory.newSession
      println("getSessionC: new session " + session + " in thread " + Thread.currentThread().getId)
      session
    } else null
  }
  var sizeCache: Int = -1
  var seCache = new mutable.HashMap[Int,SyncEntry]()
  def invalidateCache() {
    sizeCache = -1
    seCache.clear()
  }

  var syncEntries: com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[SyncEntry] = null
  var seSession: Session = null
  def getSESession = {
    if (connected) {
      if (seSession == null) seSession = sessionFactory.newSession
      println("getSESessionC: new SEsession " + seSession + " in thread " + Thread.currentThread().getId)
    }
    seSession
  }

  // TODO: probably the whole syncentry-stuff should go into own factory class
  def updateSyncEntries(onlyRelevant: Boolean) {
    if (seSession != null) {
      seSession.close
      seSession = null
    }
    syncEntries =  new com.sun.javafx.scene.control.ReadOnlyUnbackedObservableList[SyncEntry]() {
      def get(p1: Int): SyncEntry = {
        if (!seCache.contains(p1)) {
          if (seCache.size > 1000) seCache.clear()
          println("get p1=" + p1)
          using(getSESession) {
            val res = if (onlyRelevant)
              MySchema.files.where(se => se.relevant === true).page(p1,20) // TODO filtering etc....
            else
              from(MySchema.files)(s=>select(s))
            var iii = p1
            res.foreach(se => {
              if (!seCache.contains(iii)) seCache.put(iii,se)
              iii += 1
            })
          }
        }
        seCache.get(p1).getOrElse(null)
      }
      def size(): Int = {
        if (sizeCache == -1) {
          if (getSession != null)
            using(getSESession) { sizeCache = MySchema.files.where(se => se.relevant === true).size }
          else sizeCache = 0
          println("get size=" + sizeCache)
        }
        sizeCache
      }
      def toArray[T](a: Array[T]): Array[T] = null

    }
  }

  def dbpath(name: String) = DBSettings.settpath + "/cache/" + name

  val sessionFactory = SessionFactory

  def cleanup() {
    if (seSession != null) {
      seSession.close
      seSession = null
    }
    invalidateCache()
  }

  var isNewDB = false

  def connectDB(name: String) {
    cleanup()
    connected = false

    println("connecting to database name=" + name)
    Class.forName("org.h2.Driver")
    val dbexists = (Files.exists(Paths.get(dbpath(name) + ".h2.db") ))
    val databaseConnection = s"jdbc:h2:" + dbpath(name) + (if (dbexists) ";create=true" else "")
    sessionFactory.concreteFactory = Some(() => {
      Session.create(java.sql.DriverManager.getConnection(databaseConnection), new H2Adapter)
    })
    connected = true
    transaction {
      if (!dbexists) {
        isNewDB = true
        MySchema.create
        println("  Created the schema")
        // TODO: testing
//        for (i <- 1 until 100) {
//          MySchema.files.insert(new SyncEntry("asdf" + i,-1, 1,i,11,12,20,-1,true))
//        }
      }
    }
    updateSyncEntries(false)
  }

}
