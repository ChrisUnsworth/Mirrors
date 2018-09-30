package Mirrors

import scala.io.BufferedSource

class Reader(source: BufferedSource) {
  lazy val lines: Iterator[String] = source.getLines()

  def next: Option[Lock] = {
    if (!lines.hasNext) return None

    val dimensions = tryParseDimensions(lines.next()).getOrElse(return None)

    val lock = Lock(dimensions.rows, dimensions.columns)

    for ((r, c) <- lines.take(dimensions.m).map(parsePair)){
      lock.add(Mirror.rightHandMirror(r, c))
    }

    for ((r, c) <- lines.take(dimensions.n).map(parsePair)){
      lock.add(Mirror.leftHandMirror(r, c))
    }

    Some(lock)
  }

  case class LockSize(rows: Int, columns: Int, m: Int, n: Int)

  def tryParseDimensions(value: String): Option[LockSize] = {
    val ints = parseIntRow(value)
    if (ints.length != 4) None
    Some(LockSize(ints.head, ints(1), ints(2), ints(3)))
  }

  // throw error on fail as that indicates a format error in the input
  def parsePair(value: String): (Int, Int) = {
    val ints = parseIntRow(value)
    if (ints.length != 2) throw new RuntimeException("Unexpected format in input file")
    (ints.head, ints(1))
  }

  def parseIntRow(value: String): Array[Int] = {
    if (value == null) return Array()
    value.split(' ').flatMap(tryParseInt)
  }

  def tryParseInt(value: String): Option[Int] = {
    try Some(value.toInt)
    catch { case _: java.lang.NumberFormatException => None }
  }

  def close(): Unit = source.close()
}
