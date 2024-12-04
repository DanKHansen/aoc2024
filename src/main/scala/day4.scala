import scala.util.{Failure, Success, Try}

@main
def day4(): Unit = {
   val grid = getSource("4.txt")
   val lines = grid.indices
   val positions = grid.head.indices
   val directions = List("n", "ne", "e", "se", "s", "sw", "w", "nw")
   val directions2 = directions.filter(_.length == 2)

   println(s"1: ${countXmas(lines, positions, directions, "xmas", grid)}")
   println(s"2: ${countMas(lines, positions, directions2, "mas", grid)}")

   def countMas(lin: Range, pos: Range, dirs: List[String], word: String, grid: List[String]): Int =
      (for
         l <- lin
         p <- pos
      yield find(grid, dirs, -word.length / 2 to word.length / 2, l, p).map(v =>
         if (v.mkString.contains(word.toUpperCase) || v.mkString.contains(word.reverse.toUpperCase)) true else false))
         .count(l => l.forall(_ == true))

   def countXmas(lin: Range, pos: Range, dirs: List[String], word: String, grid: List[String]): Int =
      (for
         l <- lin
         p <- pos
      yield find(grid, dirs, 0 until word.length, l, p).map(v =>
         if (v.mkString == word.toUpperCase) true else false)).flatten.count(_ == true)

   def star(pos: Int, lin: Int, rng:Range): Map[String, Seq[(Int, Int)]] =
      Map(
        "n" -> (for i <- rng yield (pos, lin - i)),
        "ne" -> (for i <- rng yield (pos + i, lin - i)),
        "e" -> (for i <- rng yield (pos + i, lin)),
        "se" -> (for i <- rng yield (pos + i, lin + i)),
        "s" -> (for i <- rng yield (pos, lin + i)),
        "sw" -> (for i <- rng yield (pos - i, lin + i)),
        "w" -> (for i <- rng yield (pos - i, lin)),
        "nw" -> (for i <- rng yield (pos - i, lin - i))
      )

   def find(grid: List[String], dirs: List[String], rng:Range, lin: Int, pos: Int): List[IndexedSeq[Char]] =
      dirs.map { dir =>
         val v = star(pos, lin, rng).getOrElse(dir, Nil)
         for i <- v.indices
         yield Try(grid(v(i)._2)(v(i)._1)) match
            case Failure(_)     => ' '
            case Success(value) => value
      }

}
