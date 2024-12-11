import scala.annotation.tailrec

@main
def day9(): Unit = {
   val src = getSource("9.txt")
   val list = src.mkString.grouped(2).zipWithIndex
   val parsed = list.flatMap { case (fileSize, idx) =>
      val size = fileSize.head.asDigit
      val dotRepeat = if (fileSize.length == 1) 0 else fileSize.last.asDigit
      val idxString = (idx.toString + " ") * size
      val dots = ". " * dotRepeat
      (idxString + dots).split(" ")
   }.toList

   def moveFilesFromBackToEmpty(l: List[String]): List[String] = {
      @tailrec
      def loop(in: List[String]): List[String] = {
         val idxOfFirstDot = in.indexOf(".")
         val lastNonDot = in.filterNot(_ == ".").lastOption
         lastNonDot match {
            case Some(last) =>
               val idxOfLastNumber = in.lastIndexOf(last)
               if idxOfFirstDot >= idxOfLastNumber then in
               else
                  val next = in.updated(idxOfFirstDot, last).updated(idxOfLastNumber, ".")
                  loop(next)
            case None => in
         }
      }
      loop(l)
   }

   def moveFilesForward(l: List[String]): List[String] = ???

   def multiply(p: (String, Int)): Long =
      val (str, num) = p
      str match
         case "." => 0L
         case _   => str.toLong * num

   def checkSum(l: List[(String, Int)]) = l.map(multiply).sum

   println(s"1: ${checkSum(moveFilesFromBackToEmpty(parsed).filterNot(_ == ".").zipWithIndex)}")
   //println(s"2: ${checkSum(moveFilesForward(parsed).zipWithIndex)}")
}
