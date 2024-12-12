import scala.annotation.tailrec

@main
def day9(): Unit = {
   type Segment = List[String]
   type SegList = List[Segment]

   val src = getSource("9_test.txt")
   val list = src.mkString.grouped(2).zipWithIndex
   lazy val parsed = list.flatMap { case (fileSize, idx) =>
      val size = fileSize.head.asDigit
      val dotRepeat = if (fileSize.length == 1) 0 else fileSize.last.asDigit
      val idxString = (idx.toString + " ") * size
      val dots = ". " * dotRepeat
      (idxString + dots).split(" ")
   }.toList
   lazy val disk: SegList = segments(parsed)

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

   def moveSegmentsForward(dsk: SegList): Segment = {
      @tailrec
      def loop(in: SegList, idx: Int): Segment =
         val usedSlots = in.filterNot(_.forall(_ == "."))
         val freeSlots = in.filter(_.forall(_ == ".")).takeWhile(in.indexOf(_) < in.indexOf(usedSlots(idx)))
         if freeSlots.isEmpty || idx == 0 then in.flatten
         else
            val availableSlots = freeSlots.filter(_.size >= usedSlots(idx).size)
            if availableSlots.isEmpty then loop(in, idx - 1)
            else
               val newSlot =
                  if usedSlots(idx).size < availableSlots.head.size then
                     usedSlots(idx).padTo(availableSlots.head.size, '.').map(_.toString)
                  else usedSlots(idx)
               val newDisk: SegList = segments(
                 in.updated(in.indexOf(availableSlots.head), newSlot)
                    .updated(
                      in.indexOf(usedSlots(idx)),
                      usedSlots(idx)
                         .patch(0, "." * usedSlots(idx).size, 10)
                         .map(_.toString))
                    .flatten)
               loop(newDisk, idx - 1)
      loop(dsk, dsk.filterNot(_.flatten.contains('.')).length - 1)
   }


   def multiply(p: (String, Int)): Long =
      val (str, num) = p
      str match
         case "." => 0L
         case _   => str.toLong * num

   def checkSum(l: List[(String, Int)]) = l.map(multiply).sum

   println(s"1: ${checkSum(moveSegmentBackToEmpty(parsed).filterNot(_ == ".").zipWithIndex)}")
   println(s"2: ${checkSum(moveSegmentsForward(disk).zipWithIndex)}")
}
