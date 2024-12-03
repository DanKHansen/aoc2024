@main
def day2(): Unit = {
   val reports = getSource("2.txt").map(str => str.split(" ").toList.map(_.toInt))

   def listOfDiffs(report: List[Int]) = report.sliding(2).map { case Seq(lvl1, lvl2) => lvl2 - lvl1 }.toList

   def isReportSafe(report: List[Int]): Boolean = {
      val diffs = listOfDiffs(report)
      (diffs.forall(_ > 0) || diffs.forall(_ < 0))
      && diffs.forall(diff => math.abs(diff) <= 3)
      && !diffs.contains(0)
   }

   def isReportStillSafe(report: List[Int]): Boolean =
      isReportSafe(report) || report.indices.exists(idx => isReportSafe(report.patch(idx, Nil, 1)))

   val first = reports.map(isReportSafe).count(_ == true)
   val second = reports.map(isReportStillSafe).count(_ == true)

   println(first)
   println(second)

}
