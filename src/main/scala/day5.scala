import sources._
import scala.annotation.tailrec
object day5 {
   def main(args: Array[String]): Unit = {
      val source = getSource("5.txt")
      val rules: Vector[(Int, Int)] =
         source
            .takeWhile(_ != "")
            .map { line =>
               val Array(first, last) = line.split("\\|").map(_.toInt)
               (first, last)
            }
            .toVector
      val updates: Seq[Vector[Int]] =
         source.dropWhile(_ != "").tail.map(_.split(",").map(_.toInt).toVector)
      println(s"1: ${updates.filter(isUpdateValid).map(getMiddleNumber).sum}")
      println(s"2: ${updates.filterNot(isUpdateValid).map(rearrange).map(getMiddleNumber).sum}")

      def getMiddleNumber(v: Vector[Int]): Int = v(v.length / 2)

      def isValidPair(x: Int, y: Int): Boolean = rules.contains((x, y))

      @tailrec
      def isUpdateValid(v: Vector[Int]): Boolean =
         if (v.length <= 1) true
         else {
            val head = v.head
            val tail = v.tail
            if (tail.forall(i => rules.contains((head, i)))) isUpdateValid(tail)
            else false
         }
      def rearrange(update: Vector[Int]): Vector[Int] = {
         @tailrec
         def checkPairs(v: Vector[Int]): Vector[Int] = {
            val pairs = v.sliding(2).toVector
            val (_, invalidPairs) = pairs.partition { case Vector(first, second) => isValidPair(first, second) }

            if (invalidPairs.isEmpty) v
            else {
               val firstInvalidPair = invalidPairs.head
               val swapped = v
                  .updated(v.indexOf(firstInvalidPair(0)), firstInvalidPair(1))
                  .updated(v.indexOf(firstInvalidPair(1)), firstInvalidPair(0))
               checkPairs(swapped)
            }
         }
         checkPairs(update)
      }
   }
}
