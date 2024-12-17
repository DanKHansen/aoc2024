import sources._
import scala.util.Try
object day4 {
   def main(args: Array[String]): Unit = {
      val grid = getSource("4.txt")
      val dirs = List("n", "ne", "e", "se", "s", "sw", "w", "nw")
      println(s"1: ${countXmas("xmas")}")
      println(s"2: ${countWords("mas")}")

      def countWords(word: String) =
         val wordUpper = word.toUpperCase
         val rng = -word.length / 2 to word.length / 2
         grid.indices
            .flatMap(l => grid.head.indices.map(p => find(dirs.filter(_.length == 2), rng, l, p)))
            .map(l => l.count(_ == wordUpper.toVector))
            .count(_ == 2)

      def countXmas(word: String): Int =
         val wordUpper = word.toUpperCase
         val rng = 0 until word.length
         grid.indices.flatMap { l =>
            grid.head.indices.flatMap { p =>
               find(dirs, rng, l, p)
                  .filter(_.mkString == wordUpper)
            }
         }.size

      def star(pos: Int, lin: Int, rng: Range): Map[String, Seq[(Int, Int)]] =
         dirs.map { dir =>
            dir -> (for (i <- rng) yield dir match {
               case "n"  => (pos, lin - i)
               case "ne" => (pos + i, lin - i)
               case "e"  => (pos + i, lin)
               case "se" => (pos + i, lin + i)
               case "s"  => (pos, lin + i)
               case "sw" => (pos - i, lin + i)
               case "w"  => (pos - i, lin)
               case "nw" => (pos - i, lin - i)
            })
         }.toMap

      def find(dirs: List[String], rng: Range, lin: Int, pos: Int): List[Seq[Char]] =
         dirs.map { dir =>
            star(pos, lin, rng)
               .getOrElse(dir, Nil)
               .map { case (x, y) =>
                  Try(grid(y)(x)).getOrElse(' ')
               }
         }
   }
}
