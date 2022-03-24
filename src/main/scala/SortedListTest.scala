import SortedList._

object SortedListTest {
  def main(args: Array[String]): Unit = {
    val empty = SortedList.Nil[Int]()
    val one = 1 :: SortedList.Nil[Int]()
    val two = one.add(0)
    val three = SortedList(3, 2, 1)
    println(one.isEmpty)
    println(two.add(2))
    three.foreach(println)
    println(empty.isEmpty)
    println(empty)

    try {
      empty.head
    }
    catch {
      case e: Exception => println(e)
    }
  }
}

