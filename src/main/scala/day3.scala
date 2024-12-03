@main
def day3(): Unit = {
   val memory = getSource("3.txt").mkString
   val pattern = "mul\\((\\d+),(\\d+)\\)".r
   val removePattern = "don't\\(\\)(.*?)(do\\(\\)|$)"

   def mul(x: Int, y: Int) = x * y

   val first = pattern.findAllMatchIn(memory).foldLeft(0) { case (sum, pat) =>
      sum + mul(pat.group(1).toInt, pat.group(2).toInt)
   }

   val second = pattern.findAllMatchIn(memory.replaceAll(removePattern, "")).foldLeft(0) { case (sum, pat) =>
      sum + mul(pat.group(1).toInt, pat.group(2).toInt)
   }

   println(first)
   println(second)

}
