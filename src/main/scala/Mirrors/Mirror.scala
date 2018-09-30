package Mirrors

import Mirrors.Beam.Beam

object Mirror {
  def rightHandMirror(row: Int, column: Int): Mirror = RightHandMirrorImpl(row, column)
  def leftHandMirror(row: Int, column: Int): Mirror = LeftHandMirrorImpl(row, column)
  def source(firstInRow: Option[Mirror]): Mirror = LaserSourceImpl(firstInRow)
  def sink(rows: Int, columns: Int, lastInRow: Option[Mirror]): Mirror = LaserSinkImpl(rows, columns, lastInRow)

  case class RightHandMirrorImpl(row: Int, column: Int) extends Mirror {
    override def reflect(beam: Beam): Beam = {
      beam match {
        case Beam.North => Beam.East
        case Beam.East => Beam.North
        case Beam.West => Beam.South
        case Beam.South => Beam.West
      }
    }
  }

  case class LeftHandMirrorImpl(row: Int, column: Int) extends Mirror {
    override def reflect(beam: Beam): Beam = {
      beam match {
        case Beam.North => Beam.West
        case Beam.East => Beam.South
        case Beam.West => Beam.North
        case Beam.South => Beam.East
      }
    }
  }

  case class LaserSourceImpl(firstInRow: Option[Mirror]) extends Mirror {
    override def row: Int = 1
    override def column: Int = 0
    override def reflect(beam: Beam): Beam = Beam.East
    rowChild = firstInRow
  }

  case class LaserSinkImpl(rows: Int, columns: Int, lastInRow: Option[Mirror]) extends Mirror {
    override def row: Int = rows
    override def column: Int = columns + 1
    override def reflect(beam: Beam): Beam = Beam.West
    rowParent = lastInRow
  }
}

trait Mirror {
  def row: Int
  def column: Int
  def reflect(beam: Beam): Beam
  def nextHit(beam: Beam): Option[Mirror] = {
    beam match {
      case Beam.North => columnParent
      case Beam.East => rowChild
      case Beam.West => rowParent
      case Beam.South => columnChild
    }
  }

  def iterateRow: Stream[Mirror] = {
    if (rowChild.isEmpty) this #:: Stream.empty[Mirror]
    else this #:: rowChild.get.iterateRow
  }

  def iterateColumn: Stream[Mirror] = {
    if (columnChild.isEmpty) this #:: Stream.empty[Mirror]
    else this #:: columnChild.get.iterateColumn
  }

  var rowParent: Option[Mirror] = None
  var rowChild: Option[Mirror] = None
  var columnParent: Option[Mirror] = None
  var columnChild: Option[Mirror] = None
}
