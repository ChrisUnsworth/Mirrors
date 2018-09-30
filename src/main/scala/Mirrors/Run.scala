package Mirrors

import scala.io.Source

object Run {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) throw new IllegalArgumentException("First argument should be an input file path. From commandline: sbt \"run <filepath>\". The quote marks are required.")
    val fileName = args.head
    var reader: Reader = null
    try {
      reader = new Reader(Source.fromFile(fileName))

      var lock = reader.next
      var counter = 1
      while (lock.isDefined){
        val checkResult = LockChecker.check(lock.get)
        val report = checkResult match {
          case r if !r.defaultToLocked => "0"
          case r if r.keys.isEmpty => "impossible"
          case _ =>
            val key = checkResult.keys.minBy(k => (k._1, k._2))
            s"${checkResult.keys.size} ${key._1} ${key._2}"
        }

        println(s"Case $counter: $report")
        lock = reader.next
        counter += 1
      }
    }
    finally {
      if (reader != null) reader.close()
    }
  }
}
