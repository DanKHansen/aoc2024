import scala.annotation.tailrec

@main
def day9(): Unit = {
   val src = getSource("9.txt")
   val list = src.mkString.grouped(2).zipWithIndex
   val parsed = list
      .map { case (fileSize, idx) =>
         val size = fileSize.head.toString.toInt
         val repeatCount = if (fileSize.length == 1) 0 else fileSize.last.toString.toInt
         ((idx.toString + " ") * size).split(" ") ++ ("." * repeatCount)
      }
      .flatMap(_.toList)
      .map(_.toString)
      .toList

   def moveFilesFromBackToEmpty(l: List[String]): List[String] = {
      @tailrec
      def loop(in: List[String]): List[String] = {
         val idxOfLastNumber = in.lastIndexOf(in.filterNot(_ == ".").last)
         val idxOfFirstDot = in.indexOf(".")
         if idxOfFirstDot < idxOfLastNumber then {
            val next = in.updated(idxOfFirstDot, in(idxOfLastNumber)).updated(idxOfLastNumber, ".")
            loop(next)
         } else in
      }
      loop(l)
   }

   def multiply(p: (String, Int)): Long = p._1.toLong * p._2

   def checkSum(l: List[(String, Int)]) = l.map(multiply).sum

   println(s"1: ${checkSum(moveFilesFromBackToEmpty(parsed).filterNot(_ ==".").zipWithIndex)}")
   // println(s"2: ${}")
}
