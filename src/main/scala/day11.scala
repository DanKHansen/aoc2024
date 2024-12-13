@main
def day11(): Unit = {
   val src = getSource("11.txt")

   object Even:
      def unapply(n: Long): Option[(Long, Long)] = splitDigits(n)

   extension (stones: Map[Long, Long])
      def diff(stone: Long, change: Long): Map[Long, Long] =
         stones.updatedWith(stone):
            case None => Some(change)
            case Some(n) =>
               val n0 = n + change
               if n0 == 0 then None else Some(n0)

   def blink(stones: Seq[Long]): Seq[Long] =
      stones flatMap:
         case 0          => 1 :: Nil
         case Even(x, y) => x :: y :: Nil
         case z          => z * 2024 :: Nil

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

   def blinkUnordered(stones: Map[Long, Long]): Map[Long, Long] =
      stones.foldLeft(stones): (nextStones, stone) =>
         stone match
            case (0, n)                => nextStones.diff(0, -n).diff(1, n)
            case (old @ Even(a, b), n) => nextStones.diff(old, -n).diff(a, n).diff(b, n)
            case (other, n)            => nextStones.diff(other, -n).diff(other * 2024, n)

   val stones: Seq[Long] = src.mkString.split(" ").map(_.toLong).toSeq
   val stones2: Map[Long, Long] = stones.groupBy(identity).map((k, v) => (k, v.size.toLong))

   println(s"1: ${Iterator.iterate(stones)(blink).drop(25).next.size}")
   println(s"2: ${Iterator.iterate(stones2)(blinkUnordered).drop(75).next.values.sum}")
}
