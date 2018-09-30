package Mirrors

import java.io.ByteArrayInputStream

import org.scalatest.exceptions.TestFailedException
import org.scalatest.{FunSuite, Matchers}

import scala.io.BufferedSource

class ReaderTest extends FunSuite with Matchers {

  test ("Read one lock") {
    val definition =
      """5 6 1 4
        |2 3
        |1 2
        |2 5
        |4 2
        |5 5""".stripMargin

    val stream = new ByteArrayInputStream(definition.getBytes)
    val reader = new Reader(new BufferedSource(stream))
    val lockOption = reader.next
    lockOption.isDefined shouldBe true
    val lock = lockOption.get
    lock.rowCount shouldBe 5
    lock.columnCount shouldBe 6
  }

  test ("Read three locks") {
    val definition =
      """5 6 1 4
        |2 3
        |1 2
        |2 5
        |4 2
        |5 5
        |100 100 0 2
        |1 77
        |100 77
        |100 100 0 0""".stripMargin

    val stream = new ByteArrayInputStream(definition.getBytes)
    val reader = new Reader(new BufferedSource(stream))
    val lock1 = reader.next
    lock1.isDefined shouldBe true
    val lock2 = reader.next
    lock2.isDefined shouldBe true
    val lock3 = reader.next
    lock3.isDefined shouldBe true
    val lock4 = reader.next
    lock4.isEmpty shouldBe true
  }

  test ("tryParseDimensions") {
    val reader = new Reader(null)
    val result = reader.tryParseDimensions("5 6 1 4")
    result.isDefined shouldBe true
    val dimensions = result.get
    dimensions.rows shouldBe 5
    dimensions.columns shouldBe 6
    dimensions.m shouldBe 1
    dimensions.n shouldBe 4
  }

  test ("parsePair") {
    val reader = new Reader(null)
    val (x, y) = reader.parsePair("5 6")
    x shouldBe 5
    y shouldBe 6

    try {
      reader.parsePair("")
      fail()
    } catch {
      case f: TestFailedException => fail()
      case e: RuntimeException => e should not be null
    }
  }
}
