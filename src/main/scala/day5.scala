import scala.annotation.tailrec

@main
def day5(): Unit = {
   val source = getSource("5.txt")
   val rules: Seq[(Int, Int)] =
      source.takeWhile(_ != "").map { line =>
         val Array(first, last) = line.split("\\|").map(_.toInt)
         (first, last)
      }
   val updates: Seq[Vector[Int]] =
      source.dropWhile(_ != "").tail.map(_.split(",").map(_.toInt).toVector)

   def getMiddleNumber(v: Vector[Int]): Int = v(v.length / 2)

   @tailrec
   def checkFwd(v: Vector[Int], r: Seq[(Int, Int)]): Boolean =
      if (v.isEmpty) true
      else {
         val head = v.head
         val tail = v.tail
         if (tail.forall(i => r.contains((head, i)))) checkFwd(tail, r)
         else false
      }

   println(s"1: ${updates.map(u => if (checkFwd(u, rules)) getMiddleNumber(u) else 0).sum}")
   // println(s"2: ${}")

}
