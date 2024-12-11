import scala.annotation.tailrec

@main
def day9(): Unit = {
   type Segment = List[String]
   val src = getSource("9_test.txt")
   val list = src.mkString.grouped(2).zipWithIndex
   val parsed: Segment = list.flatMap { case (fileSize, idx) =>
      val size = fileSize.head.asDigit
      val dotRepeat = if (fileSize.length == 1) 0 else fileSize.last.asDigit
      val idxString = (idx.toString + " ") * size
      val dots = ". " * dotRepeat
      (idxString + dots).split(" ")
   }.toList


   def segments(inputList: Segment): List[Segment] = {
      var result = List[List[String]]()
      var currentSection = List[String]()
      for (item <- inputList) {
         if (currentSection.isEmpty || currentSection.last == item) {
            currentSection = currentSection :+ item
         } else {
            result = result :+ currentSection
            currentSection = List(item)
         }
      }
      if (currentSection.nonEmpty) {
         result = result :+ currentSection
      }
      result
   }

   def moveFilesFromBackToEmpty(disk: List[String]): List[String] = {
      @tailrec
      def loop(l: List[String]): List[String] = {
         val idxOfFirstDot = l.indexOf(".")
         val lastNonDot = l.filterNot(_ == ".").lastOption
         lastNonDot match {
            case Some(last) =>
               val idxOfLastNumber = l.lastIndexOf(last)
               if idxOfFirstDot >= idxOfLastNumber then l
               else
                  val next = l.updated(idxOfFirstDot, last).updated(idxOfLastNumber, ".")
                  loop(next)
            case None => l
         }
      }
      loop(disk)
   }

//   def moveFilesForward(ss: List[Segment]): List[String] = {
//      @tailrec
//      def loop(in : List[Segment]): List[String] = ???
//
//      loop(ss)
//   }


   def multiply(p: (String, Int)): Long =
      val (str, num) = p
      str match
         case "." => 0L
         case _   => str.toLong * num

   def checkSum(l: List[(String, Int)]) = l.map(multiply).sum

   println(s"1: ${checkSum(moveFilesFromBackToEmpty(parsed).filterNot(_ == ".").zipWithIndex)}")
   //println(s"2: ${checkSum(moveFilesForward(parsed).zipWithIndex)}")
}
