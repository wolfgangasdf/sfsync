package sfsync.store

import com.db4o.config.EmbeddedConfiguration
import com.db4o.Db4oEmbedded
import com.db4o.ObjectSet
import com.db4o.config.annotations.Indexed
import com.db4o.ObjectContainer
import com.db4o.query.Predicate
import java.io.File
import collection.mutable.{ArrayBuffer, ListBuffer}
import com.db4o.diagnostic.{Diagnostic, DiagnosticListener}
import sfsync.synchro.VirtualFile
import scalafx.{collections => sfxc}
import collection.mutable

object DBSettings {
  def dbpath = "/tmp/sfsyncdb4otest.db4o"
}

//object MyImplicits {
//  // attempt to wrap OAB to OB
//  implicit def wrapObsBuf[T](value: ObsArrBuff[T]) = {
//    value.asInstanceOf[ObservableBuffer[T]]
//  }
//}
//class ObsArrBuff[T] extends ArrayBuffer[T] with ObservableBuffer[T] {
//  def toObsBuff = this.asInstanceOf[ObservableBuffer[T]]
//}



class Config {
  //  var servers = new sfxc.ObservableBuffer[Server] // THIS does not work :-( if the servers is used by ListView, it crashes the DB.....
  var servers = new mutable.ArrayBuffer[Server] //
  var currentServer = -1;
}

class Server {
  @Indexed var name: String = "<new>"
  var localFolder: String = ""
  var protocols = new mutable.ArrayBuffer[Protocol]
  var currentProtocol = -1;
  var subfolders = new mutable.ArrayBuffer[String]
  var currentSubFolder = -1;
  var cache = new mutable.ArrayBuffer[VirtualFile]

  override def toString: String = name // used for listview
}

class Protocol {
  @Indexed var name: String = "<new>"
  var protocol: String = ""
  var username : String = ""
  var password: String = ""
  var basefolder: String = ""
  override def toString: String = name
}

object Store {
  // from http://www.matthewtodd.info/?p=68
  implicit def toPredicate[T](predicate: T => Boolean) =
    new Predicate[T]() { def `match`(entry: T): Boolean = { predicate(entry) } }
  // to iterate over objects
  class RichObjectSet[T](objectSet: ObjectSet[T]) extends Iterator[T] {
    def hasNext: Boolean = objectSet.hasNext()
    def next: T = objectSet.next()
  }
  implicit def toRichObjectSet[T](objectSet: ObjectSet[T]) =
    new RichObjectSet[T](objectSet)

  var config : Config = null
  val db = DB.getsession

  def save {
    println("-----------save " + config)
//    scala.sys.error("asdf")
    dumpConfig
    db.store(config)
    db.commit()
    println("-----------/save")
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

  // Load config
  println("Store ini!!!!!!!!!!!!")
  val tmp = (db query { classOf[Config] })
  println("  configs in db: " + tmp.size() )
  if (tmp.size() > 0)
    config = tmp.get(0)
  else {
    println("       but store is empty!" + tmp)
    config = new Config {
      currentServer = -1
    }
  }
}


object DB {
  val cfg : EmbeddedConfiguration = Db4oEmbedded.newConfiguration()
  cfg.common().objectClass(classOf[Server]).cascadeOnDelete(true);
  cfg.common().objectClass(classOf[Protocol]).cascadeOnDelete(true);
  cfg.common().activationDepth(20); // important! or do it right (see transparent activation....)
  cfg.common().updateDepth(20); // VERY important
//  cfg.common().objectClass(classOf[Config]).cascadeOnUpdate(true);
//  cfg.common().objectClass(classOf[Server]).cascadeOnUpdate(true);
//  cfg.common().exceptionsOnNotStorable(false) // TODO: dangerous

  // debug DB
//  cfg.common.diagnostic.addListener(new DiagnosticListener {
//    override def onDiagnostic(diagnostic : Diagnostic) {
//      println(diagnostic)
//    }
//  })

  val rootc = Db4oEmbedded.openFile(cfg,DBSettings.dbpath)
  private val session = rootc.ext.openSession // NEVER CLOSE THIS until app quit!!!! objects are forgotten that they are stored
  def getsession(): ObjectContainer = {
    assert(!session.ext().isClosed)
    session
  }

  override def finalize() {
    session.close
    rootc.close
  }
}

// test it
object TestStore extends App {

  // from http://www.matthewtodd.info/?p=68
  implicit def toPredicate[T](predicate: T => Boolean) =
    new Predicate[T]() { def `match`(entry: T): Boolean = { predicate(entry) } }
  // to iterate over objects
  class RichObjectSet[T](objectSet: ObjectSet[T]) extends Iterator[T] {
    def hasNext: Boolean = objectSet.hasNext()
    def next: T = objectSet.next()
  }
  implicit def toRichObjectSet[T](objectSet: ObjectSet[T]) =
    new RichObjectSet[T](objectSet)


    // test it
  1 match {
    case 1 => {
      println("---------------- delete DB")
      new File(DBSettings.dbpath).delete()
    }
    case 2 => {
//      Store
      val snew = new Server {
        name = "serverXXXYZA"
        localFolder = "/tmp/testlocal/"
        protocols += new Protocol { name = "prot1" }
        protocols += new Protocol { name = "prot2" }
        subfolders += ("sf1","sf2")
      }
//      val db = DB.getsession
//      db.store(snew)
      Store.config.servers += snew
      Store.save
//      db.store(Store.config)
//      db.commit()
//      db.close()
    }
    case 5 => {
      Store.dumpConfig
    }
    case 10 => {
      new File(DBSettings.dbpath).delete()
      val cc = new Config {
        servers += new Server { name = "serva" }
        servers += new Server { name = "servb" }
      }
      val db = DB.getsession
      db.store(cc)
      db.close()
    }
    case 11 => {
      var db = DB.getsession
      val tmp = (db query { classOf[Config] })
      println("configlen=" + tmp.size())
      val cc: Config = tmp.get(0)
      println("config=" + cc)
      // THAT KILLS IT:
//      db.close()
//      db = DB.getsession
      val snew = new Server { name = "servc" }
      cc.servers += snew
      db.store(cc)
//      db.store(snew)
      db.close()
    }
    case 12 => {
      val db = DB.getsession
      val tmp = (db query { classOf[Config] })
      println("configlen=" + tmp.size())
      val cc: Config = tmp.get(0)
      println("config=" + cc)
      cc.servers.foreach(ss => println("server: " + ss))
      println("all servers=")
      val tmps = (db query { classOf[Server] })
      tmps.foreach(ss => println("server: " + ss))
      db.close()
    }
  }


  DB.rootc.close()
}
