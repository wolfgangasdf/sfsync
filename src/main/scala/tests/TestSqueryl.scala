package tests

import java.nio.file._
import attribute.BasicFileAttributes
import util.StopWatch
import java.io.IOException
import org.squeryl.adapters.{H2Adapter, DerbyAdapter}
import org.squeryl.PrimitiveTypeMode._
import org.squeryl.{Schema, KeyedEntity, Session, SessionFactory}
import scala.Some


object TestSqueryl extends App {

  val db = "h2" // h2, derby

  val dbpath = "/tmp/squerylexample" + db

  class BaseEntity extends KeyedEntity[Long] {
    var id: Long = 0
  }
  class SyncEntry(var email: String, var password: String) extends BaseEntity {
    // Zero argument constructor required
//    def this() = this("", "")
  }

  object MySchema extends Schema {
    val files = table[SyncEntry]

    on(files)(file => declare(
      file.id is (primaryKey,autoIncremented),
      file.email is (unique)))
  }

  // delete first
  val pdir = Paths.get(dbpath)
  if (Files.exists(pdir)) {
    Files.walkFileTree(pdir, new SimpleFileVisitor[Path] {
      override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }

      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }
    })
  }

  var databaseConnection = ""
  if (db == "derby") {
    Class.forName("org.apache.derby.jdbc.EmbeddedDriver")
    //  val databaseUsername = "squeryl-example"
    //  val databasePassword = "squeryl-example"
    databaseConnection = s"jdbc:derby:$dbpath;create=true"
    SessionFactory.concreteFactory = Some(() => {
      Session.create(java.sql.DriverManager.getConnection(databaseConnection), new DerbyAdapter)
    })
  }
  if (db == "h2") {
    Class.forName("org.h2.Driver")
    databaseConnection = s"jdbc:h2:$dbpath;create=true"
    SessionFactory.concreteFactory = Some(() => {
      Session.create(java.sql.DriverManager.getConnection(databaseConnection), new H2Adapter)
    })
  }

  transaction {
    MySchema.create
    println("Created the schema")
  }

  def benchmarkstore() {
    val session = SessionFactory.newSession
    for (iii <- 1 until 10000) {
      using(session) {
        val user1: SyncEntry = new SyncEntry("no" + iii, "pass")
        MySchema.files.insert(user1)
      }
    }
    session.close
    // derby: 5s h2: 2.1s
//    transaction {
//      for (iii <- 1 until 1000) {
//        val user1: SyncEntry = new SyncEntry("no" + iii, "pass")
//        MySchema.files.insert(user1)
//      }
//    }
    // derby: similar(4.6s), h2: faster! 1s
//  transaction {
//      val list = List.range(1,10000).map(x => new SyncEntry("no" + x, "pass"))
//      MySchema.files.insert(list)
//    }
  }

  def benchmarkget() { // fast: 50ms!
    transaction {
      val queriedUser: SyncEntry = MySchema.files.where(file => file.email === "no1234").head
      println(queriedUser.id + " -- " + queriedUser.email)
    }
  }

  StopWatch.timed("********************* stored in ") {
    benchmarkstore()
  }
  StopWatch.timed("********************* queried (benchmarkget) in ") {
    benchmarkget()
  }


}
