import sources._
object day2 {
   def main(args: Array[String]): Unit =
      val reports = getSource("2.txt").map(_.split(" ").toList.map(_.toInt))
      println(s"1: ${reports.map(isReportSafe).count(_ == true)}")
      println(s"2: ${reports.map(isReportStillSafe).count(_ == true)}")

   private def listOfDiffs(report: List[Int]): List[Int] =
      report.sliding(2).map { case Seq(lvl1, lvl2) => lvl2 - lvl1 }.toList

   private def isReportSafe(report: List[Int]): Boolean =
      val diffs = listOfDiffs(report)
      (diffs.forall(_ > 0) || diffs.forall(_ < 0))
      && diffs.forall(diff => math.abs(diff) <= 3)
      && !diffs.contains(0)

   private def isReportStillSafe(report: List[Int]): Boolean =
      isReportSafe(report) || report.indices.exists(idx => isReportSafe(report.patch(idx, Nil, 1)))
}
