@main
def day3(): Unit = {
   val memory = getSource("3.txt").mkString
   val pattern = "mul\\((\\d+),(\\d+)\\)".r
   def mul(x: Int, y: Int) = x * y

   val first = pattern.findAllMatchIn(memory).foldLeft(0) { case (sum, pat) =>
      sum + mul(pat.group(1).toInt, pat.group(2).toInt)
   }

   val second = ???//implement do and don't

   println(first)
   println(second)

}
