import scala.annotation.tailrec

@main
def day2(): Unit = {
   val reports = getSource("2_test.txt") map (s => s.split(" ").toList.map(_.toInt))

   def listOfDiffs(l: List[Int]) = l.sliding(2).map { case Seq(a, b) => b - a }.toList

   def isReportSafe(l: List[Int]): Boolean = {
      val ll = listOfDiffs(l)
      (ll.forall(_ > 0) || ll.forall(_ < 0))
      && ll.forall(diff => math.abs(diff) <= 3)
      && !ll.contains(0)
   }


   @tailrec
   def isReportStillSafe(l: List[Int]): Boolean =
      if isReportSafe(l) then true else
         isReportStillSafe(l.tail)
         // try to remove one level at a time and re-check the remaining report
         // if all possibilities have been exhausted return false


   val first = reports.map(isReportSafe).count(_ == true)

   val second = reports.map(r => isReportStillSafe(r)).count(_ == true)

   println(first)
   println(second)

}
