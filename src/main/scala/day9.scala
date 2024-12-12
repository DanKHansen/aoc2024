import scala.annotation.tailrec

@main
def day9(): Unit = {
   type Block = String
   type Segment = List[Block]
   type SegList = List[Segment]

   val src: Segment = getSource("9_test.txt")
   val list: Iterator[Block] = src.mkString.grouped(2)
   lazy val parsed: List[Block] = list.zipWithIndex.flatMap { case (block, index) =>
      val fileSize = block.head.asDigit
      val repeat = if (block.length == 1) 0 else block.last.asDigit
      val idx = (index.toString + " ") * fileSize
      val dots = ". " * repeat
      (idx + dots).split(" ")
   }.toList

   def segments(segment: Segment): SegList = {
      def foldLeft(sl: SegList, s: Segment, b: Block): (SegList, Segment) =
         if (s.isEmpty || s.last == b) (sl, s :+ b)
         else (sl :+ s, List(b))
      @tailrec
      def loop(sl: SegList, bl: Segment, acc: Segment): SegList =
         acc match {
            case Nil     => if (bl.nonEmpty) sl :+ bl else sl
            case b :: bs => loop(foldLeft(sl, bl, b)._1, foldLeft(sl, bl, b)._2, bs)
         }
      loop(List.empty[Segment], List.empty[String], segment)
   }

   def moveBlocksForward(segment: Segment): Segment = {
      @tailrec
      def loop(s: Segment): Segment = {
         val firstDotIdx = s.indexOf(".")
         val lastNonDot = s.filterNot(_ == ".").lastOption
         lastNonDot match {
            case Some(b) =>
               val lastNumIdx = s.lastIndexOf(b)
               if firstDotIdx >= lastNumIdx then s
               else
                  val newS = s.updated(firstDotIdx, b).updated(lastNumIdx, ".")
                  loop(newS)
            case None => s
         }
      }
      loop(segment)
   }

   def moveSegmentsForward(segmentList: SegList): Segment = {
      @tailrec
      def loop(sl: SegList, idx: Int): Segment = {
         val used = sl.filterNot(_.forall(_ == "."))
         val free = sl.filter(_.forall(_ == ".")).takeWhile(sl.indexOf(_) < sl.indexOf(used(idx)))
         if (free.isEmpty || idx == 0) sl.flatten
         else {
            val avail = free.filter(_.size >= used(idx).size)
            avail match {
               case Nil => loop(sl, idx - 1)
               case head :: _ =>
                  val segment = used(idx).padTo(head.size, ".")
                  val newSegList = sl
                     .updated(sl.indexOf(head), segment)
                     .updated(sl.indexOf(used(idx)), List.fill(used(idx).size)("."))
                  loop(segments(newSegList.flatten), idx - 1)
            }
         }
      }
      loop(segmentList, segmentList.filterNot(_.forall(_ == ".")).length - 1)
   }

   def multiply(pair: (Block, Int)): Long =
      val (b, n) = pair
      b match
         case "." => 0L
         case _   => b.toLong * n

   def checkSum(segment: Segment) = segment.zipWithIndex.map(multiply).sum

   println(s"1: ${checkSum(moveBlocksForward(parsed).filterNot(_ == "."))}")
   println(s"2: ${checkSum(moveSegmentsForward(segments(parsed)))}")
}
