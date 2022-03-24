package SortedList

import java.util.NoSuchElementException
import scala.annotation.tailrec

sealed abstract class SortedList[T: Ordering] {
  val head: T
  val tail: SortedList[T]

  def isEmpty: Boolean

  def ::(elem: T): SortedList[T] = {
    val (greater, lower) = splitToLowerGreater(elem, this)
    addManyToList(greater, SortedList.Node(elem, lower))
  }

  def add(elem: T): SortedList[T] = elem :: this

  def foreach(f: T => Unit): Unit = {
    @tailrec
    def foreachHelper[U](f: T => U, currentList: SortedList[T]): Unit = {
      if (!currentList.isEmpty) {
        f(currentList.head)
        foreachHelper(f, currentList.tail)
      }
    }

    foreachHelper(f, this)
  }

  override def toString: String = {
    @tailrec
    def toStringHelper(currentList: SortedList[T], sb: StringBuilder): Unit = {
      sb ++= currentList.head.toString
      if (currentList.tail != SortedList.Nil[T]) {
        sb ++= ", "
        toStringHelper(currentList.tail, sb)
      }
    }

    val sb = new StringBuilder()
    sb ++= "SortedList("
    if (!this.isEmpty) toStringHelper(this, sb)
    sb ++= ")"
    sb.toString
  }

  // Note, that elements in a greater are returned in a reverse order than in the List
  private def splitToLowerGreater(elem: T, list: SortedList[T]): (Vector[T], SortedList[T]) = {
    @tailrec
    def splitHelper(greater: Vector[T], currentList: SortedList[T]): (Vector[T], SortedList[T]) = {
      val order = implicitly[Ordering[T]]
      if (currentList.isEmpty || order.compare(elem, currentList.head) <= 0)
        (greater, currentList)
      else
        splitHelper(currentList.head +: greater, currentList.tail)
    }

    splitHelper(Vector.empty[T], list)
  }

  @tailrec
  private def addManyToList(elems: Vector[T], list: SortedList[T]): SortedList[T] =
    if (elems.isEmpty) list else addManyToList(elems.tail, SortedList.Node(elems.head, list))
}

object SortedList {
  final case class Nil[T: Ordering]() extends SortedList[T] {
    override lazy val head: T = throw new NoSuchElementException("The head of an empty list")
    override lazy val tail: SortedList[T] = throw new UnsupportedOperationException("The tail of an empty list")

    override def isEmpty: Boolean = true
  }

  private case class Node[T: Ordering](override val head: T, override val tail: SortedList[T]) extends SortedList[T] {
    override def isEmpty: Boolean = false
  }

  def apply[T: Ordering](elems: T*): SortedList[T] = {
    @tailrec
    def applyHelper(currentElems: Seq[T], result: SortedList[T]): SortedList[T] = {
      if (currentElems.isEmpty) result else applyHelper(currentElems.tail, Node(currentElems.head, result))
    }

    applyHelper(elems.sorted(Ordering[T].reverse), Nil())
  }
}


