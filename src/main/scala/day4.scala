import scala.util.Try

@main
def day4(): Unit = {
   val grid = getSource("4.txt")
   val directions = List("n", "ne", "e", "se", "s", "sw", "w", "nw")

   def countWords(dirs: List[String], word: String, grid: List[String]) =
      val wordUpper = word.toUpperCase
      val rng = -word.length / 2 to word.length / 2
      grid.indices
         .flatMap(l =>
            grid.head.indices
               .map(p =>
                  val found = find(grid, dirs, rng, l, p)
                  (p, found))
               .map { case (_, found) =>
                  found.count(_ == wordUpper.toVector)
               })
         .count(_ == 2)

   def countXmas(dirs: List[String], word: String, grid: List[String]): Int = {
      val wordUpper = word.toUpperCase
      val rng = 0 until word.length
      grid.indices.flatMap { l =>
         grid.head.indices.flatMap { p =>
            find(grid, dirs, rng, l, p).filter(_.mkString == wordUpper)
         }
      }.size
   }

   def star(pos: Int, lin: Int, rng: Range): Map[String, Seq[(Int, Int)]] =
      directions.map { dir =>
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

   def find(grid: List[String], dirs: List[String], rng: Range, lin: Int, pos: Int): List[Seq[Char]] =
      dirs.map { dir =>
         val v = star(pos, lin, rng).getOrElse(dir, Nil)
         v.map { case (x, y) =>
            Try(grid(y)(x)).getOrElse(' ')
         }
      }

   println(s"1: ${countXmas(directions, "xmas", grid)}")
   println(s"2: ${countWords(directions.filter(_.length == 2), "mas", grid)}")

}
