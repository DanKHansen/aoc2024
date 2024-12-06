import scala.annotation.tailrec

@main
def day5(): Unit = {
   val source = getSource("5_test.txt")
   val rules: Seq[(Int, Int)] =
      source.takeWhile(_ != "").map { line =>
         val Array(first, last) = line.split("\\|").map(_.toInt)
         (first, last)
      }
   val updates: Seq[Vector[Int]] =
      source.dropWhile(_ != "").tail.map(_.split(",").map(_.toInt).toVector)

   def getMiddleNumber(v: Vector[Int]): Int = v(v.length / 2)

   @tailrec
   def isUpdateValid(v: Vector[Int], r: Seq[(Int, Int)]): Boolean =
      if (v.length <= 1) true
      else {
         val head = v.head
         val tail = v.tail
         if (tail.forall(i => r.contains((head, i)))) isUpdateValid(tail, r)
         else false
      }

   @tailrec
   def reOrderUpdate(v: Vector[Int], r: Seq[(Int, Int)]): Vector[Int] =
      if (isUpdateValid(v, r)) v
      else {
        val rearranged: Vector[Int] = ???
         reOrderUpdate(rearranged, r)
      }

   println(s"1: ${updates.filter(isUpdateValid(_, rules)).map(getMiddleNumber).sum}")
   println(s"2: ${updates.filterNot(isUpdateValid(_, rules)).map(v => reOrderUpdate(v, rules)).map(getMiddleNumber).sum}")

}
