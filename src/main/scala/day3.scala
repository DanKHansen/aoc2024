import sources._
import java.util.regex.Pattern
import scala.util.matching.Regex
object day3 {
   def main(args: Array[String]): Unit =
      val memory = getSource("3.txt").mkString
      val regx = "mul\\((\\d+),(\\d+)\\)".r
      val remove = "don't\\(\\)(.*?)(do\\(\\)|$)"
      val clean = Pattern.compile(remove).matcher(memory).replaceAll("")
      println(s"1: ${regx.findAllMatchIn(memory).map(mul).sum}")
      println(s"2: ${regx.findAllMatchIn(clean).map(mul).sum}")

   private def mul(pat: Regex.Match) = pat.group(1).toInt * pat.group(2).toInt
}
