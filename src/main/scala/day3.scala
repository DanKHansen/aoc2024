import java.util.regex.Pattern
import scala.util.matching.Regex

@main
def day3(): Unit = {
   val memory = getSource("3.txt").mkString
   val regx = "mul\\((\\d+),(\\d+)\\)".r
   val remove = "don't\\(\\)(.*?)(do\\(\\)|$)"
   val clean = Pattern.compile(remove).matcher(memory).replaceAll("")

   def mul(pat: Regex.Match) = pat.group(1).toInt * pat.group(2).toInt

   println(s"1: ${regx.findAllMatchIn(memory).map(mul).sum}")
   println(s"2: ${regx.findAllMatchIn(clean).map(mul).sum}")

}
