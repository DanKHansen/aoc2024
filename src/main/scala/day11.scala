@main
def day11(): Unit = {
   val stones = getSource("11.txt").mkString.split(" ").toVector.map(_.toLong)

   def blink(stones: Vector[Long]): Vector[Long] =
      stones flatMap:
         case 0          => 1 :: Nil
         case Even(x, y) => x :: y :: Nil
         case z          => z * 2024 :: Nil

   object Even:
      def unapply(n: Long): Option[(Long, Long)] = splitDigits(n)

   def splitDigits(n: Long): Option[(Long, Long)] =
      val digits = Iterator
         .unfold(n):
            case 0 => None
            case i => Some((i % 10, i / 10))
         .toArray
      if digits.length % 2 == 0 then
         val (a, b) = digits.reverse.splitAt(digits.length / 2)
         Some((mergeDigits(a), mergeDigits(b)))
      else None

   def mergeDigits(digits: Array[Long]): Long =
      digits.foldLeft(0L): (acc, digit) =>
         acc * 10 + digit

   println(s"1: ${Iterator.iterate(stones)(blink).drop(25).next.size}")
   // println(s"2: ${Iterator.iterate(stones)(blink).drop(40).next.size}")
}
