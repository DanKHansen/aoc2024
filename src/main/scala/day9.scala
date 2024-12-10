import scala.annotation.tailrec

@main
def day9(): Unit = {
   val src = getSource("9.txt")
   val list = src.mkString.grouped(2).zipWithIndex

   val parsed = list.map { case (fileSize, idx) =>
      val size = fileSize.head.toString.toLong
      val repeatCount = if (fileSize.length == 1) 0L else fileSize.last.toString.toLong
      idx.toString * size.toInt + "." * repeatCount.toInt
   }.mkString

   def moveFilesFromBackToEmpty(s: String): String = {
      @tailrec
      def loop(in: String): String = {
         val replaceString = in.lastOption.getOrElse(' ').toString
         val next = in.replaceFirst("\\.", replaceString).dropRight(1)
         if (next.contains('.')) loop(next) else next
      }
      loop(s)
   }

   println(parsed)

   def checkSum(v: Vector[(Char, Long)]) = v.map(p => p._1.toString.toLong * p._2).sum

   //println(s"1: ${checkSum(moveFilesFromBackToEmpty(parsed).zipWithIndex.toVector.map(p => (p._1,p._2.toLong)))}")
   // println(s"2: ${}")
}
