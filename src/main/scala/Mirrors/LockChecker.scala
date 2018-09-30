package Mirrors

import Mirrors.Beam.Beam

object LockChecker {

  def check(lock: Lock): LockCheckResult = {
    val firstMirror = lock.row(1) match {
      case m if m != null => Some(m)
      case _ => None
    }

    // single row locks are a special case
    // if there is a mirror then they are impossible
    // otherwise they open without inserting a mirror
    if (lock.rowCount == 1) {
      return LockCheckResult(firstMirror.isDefined, List())
    }

    // finds the set of unoccupied grid points the beam will pass
    val (forwardPathBeam, forwardPath) = getPath(Beam.East, Mirror.source(firstMirror), lock, List())

    val forwardSet = forwardPath.toSet

    // if the path ends on the last row facing east then the safe defaults to open
    if (forwardPathBeam == Beam.East && forwardSet.contains((lock.rowCount, lock.columnCount))){
      return LockCheckResult(defaultToLocked = false, List())
    }

    val lastMirror = lock.row(lock.rowCount) match {
      case m if m != null => Some(m.iterateRow.last)
      case _ => None
    }

    val (_, backPath) = getPath(Beam.West, Mirror.sink(lock.rowCount, lock.columnCount, lastMirror), lock, List())

    val keyLocations = forwardSet.intersect(backPath.toSet)

    LockCheckResult(defaultToLocked = true, keyLocations.toList)
  }

  // Follows the path of the beam from the given mirror
  // returns a list of unoccupied grid locations the beam passes through
  def getPath(inBeam: Beam, inMirror: Mirror, lock: Lock, soFar: List[(Int, Int)]): (Beam, List[(Int, Int)]) = {
    val nextMirror = inMirror.nextHit(inBeam)
    if (nextMirror.isEmpty) {
      return (inBeam, soFar ++ pointsToEdge(inBeam, inMirror, lock.rowCount, lock.columnCount))
    }

    val newPoints = pointsBetween(inBeam, inMirror, nextMirror.get)
    val nextBeam = nextMirror.get.reflect(inBeam)
    getPath(nextBeam, nextMirror.get, lock, soFar ++ newPoints)
  }

  // finds all grid points the beam will pass through between the two mirrors
  def pointsBetween(beam: Beam, m1: Mirror, m2: Mirror): List[(Int, Int)] = {
    beam match {
      case Beam.North => ((m2.row + 1) until m1.row).map((_, m1.column)).toList
      case Beam.East => ((m1.column + 1) until m2.column).map((m1.row, _)).toList
      case Beam.South => ((m1.row + 1) until m2.row).map((_, m1.column)).toList
      case Beam.West => ((m2.column + 1) until m1.column).map((m1.row, _)).toList
    }
  }

  // finds all grid points the beam will pass through between the mirror and the edge
  def pointsToEdge(beam: Beam, m: Mirror, rows: Int, columns: Int): List[(Int, Int)] = {
    beam match {
      case Beam.North => (1 until m.row).map((_, m.column)).toList
      case Beam.East => ((m.column + 1) to columns).map((m.row, _)).toList
      case Beam.South => ((m.row + 1) to rows).map((_, m.column)).toList
      case Beam.West => (1 until m.column).map((m.row, _)).toList
    }
  }
}
