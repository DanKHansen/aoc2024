import scala.annotation.tailrec

@main
def day9(): Unit = {
   type Segment = List[String]
   type SegList = List[Segment]

   val src = getSource("9_test.txt")
   val list = src.mkString.grouped(2).zipWithIndex
   val parsed = list.flatMap { case (fileSize, idx) =>
      val size = fileSize.head.asDigit
      val dotRepeat = if (fileSize.length == 1) 0 else fileSize.last.asDigit
      val idxString = (idx.toString + " ") * size
      val dots = ". " * dotRepeat
      (idxString + dots).split(" ")
   }.toList

   def segments(segList: Segment): SegList =
      segList.foldLeft(List.empty[Segment] -> List.empty[String]) { case ((sl, seg), s) =>
         if (seg.isEmpty || seg.last == s) {
            (sl, seg :+ s)
         } else {
            (sl :+ seg, List(s))
         }
      } match {
         case (sl, seg) =>
            if (seg.nonEmpty) sl :+ seg else sl
      }

   def moveSegmentBackToEmpty(seg: Segment): Segment = {
      @tailrec
      def loop(l: Segment): Segment = {
         val firstDotIdx = l.indexOf(".")
         val lastNonDot = l.filterNot(_ == ".").lastOption
         lastNonDot match {
            case Some(str) =>
               val lastNumIdx = l.lastIndexOf(str)
               if firstDotIdx >= lastNumIdx then l
               else
                  val newL = l.updated(firstDotIdx, str).updated(lastNumIdx, ".")
                  loop(newL)
            case None => l
         }
      }
      loop(seg)
   }

//   def moveSegmentsForward(segList: SegList): Segment = {
//      @tailrec
//      def loop(in : SegList): Segment = ???
//
//      loop(ss)
//   }

   def multiply(p: (String, Int)): Long =
      val (str, num) = p
      str match
         case "." => 0L
         case _   => str.toLong * num

   def checkSum(l: List[(String, Int)]) = l.map(multiply).sum

   println(s"1: ${checkSum(moveSegmentBackToEmpty(parsed).filterNot(_ == ".").zipWithIndex)}")
   // println(s"2: ${checkSum(moveFilesForward(parsed).zipWithIndex)}")
}
