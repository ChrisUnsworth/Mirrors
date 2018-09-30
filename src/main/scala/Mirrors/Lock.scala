package Mirrors

object Lock {
  def apply(rows: Int, columns: Int): Lock = LockImpl(Array.fill(rows + 1)(null), Array.fill(columns + 1)(null))

  case class LockImpl(rows: Array[Mirror], columns: Array[Mirror]) extends Lock {
    override def row(idx: Int): Mirror = rows(idx)
    override def column(idx: Int): Mirror = columns(idx)

    override def rowCount: Int = rows.length - 1
    override def columnCount: Int = columns.length - 1

    override def add(mirror: Mirror): Unit = {
      addToRow(mirror)
      addToColumn(mirror)
    }

    private def addToRow(mirror: Mirror): Unit = {
      val rowHead = rows(mirror.row)
      if (rowHead == null){
        rows(mirror.row) = mirror
      } else if (rowHead.column > mirror.column) {
        rows(mirror.row) = mirror
        rowHead.rowParent = Some(mirror)
        mirror.rowChild = Some(rowHead)
      } else {
        val parent = rowHead.iterateRow.dropWhile(_.rowChild.exists(_.column < mirror.column)).head
        mirror.rowChild = parent.rowChild
        mirror.rowParent = Some(parent)
        parent.rowChild.foreach(_.rowParent = Some(mirror))
        parent.rowChild = Some(mirror)
      }
    }

    private def addToColumn(mirror: Mirror): Unit = {
      val columnHead = columns(mirror.column)
      if (columnHead == null){
        columns(mirror.column) = mirror
      } else if (columnHead.row > mirror.row) {
        columns(mirror.column) = mirror
        columnHead.columnParent = Some(mirror)
        mirror.columnChild = Some(columnHead)
      } else {
        val parent = columnHead.iterateColumn.dropWhile(_.columnChild.exists(_.row < mirror.row)).head
        mirror.columnChild = parent.columnChild
        mirror.columnParent = Some(parent)
        parent.columnChild.foreach(_.columnParent = Some(mirror))
        parent.columnChild = Some(mirror)
      }
    }
  }
}

trait Lock {
  def add(mirror: Mirror): Unit
  def row(idx: Int): Mirror
  def column(idx: Int): Mirror
  def rowCount: Int
  def columnCount: Int
}
