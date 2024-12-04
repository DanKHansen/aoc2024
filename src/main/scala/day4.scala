import scala.util.{Failure, Success, Try}

@main
def day4(): Unit = {
   val grid = getSource("4.txt")
   val lines = grid.indices
   val positions = grid.head.indices
   val directions = List("n", "ne", "e", "se", "s", "sw", "w", "nw")
   val directions2 = directions.filter(_.length == 2)

   println(s"1: ${countXmas(directions, "xmas", grid)}")
   println(s"2: ${countWords(directions2, "mas", grid)}")

   def countWords(dirs: List[String], word: String, grid: List[String]) =
      val rng = -word.length / 2 to word.length / 2
      (for
         l <- grid.indices
         p <- grid.head.indices
      yield find(grid, dirs, rng, l, p))
         .map(l => l.count(_ == word.toUpperCase.toVector) == 2 && l.count(_ == word.toUpperCase.toVector) == 2)
         .count(_ == true)

   def countXmas(dirs: List[String], word: String, grid: List[String]): Int =
      val rng = 0 until word.length
      (for
         l <- grid.indices
         p <- grid.head.indices
      yield find(grid, dirs, rng, l, p).map(v => v.mkString == word.toUpperCase)).flatten
         .count(_ == true)

   def star(pos: Int, lin: Int, rng: Range): Map[String, Seq[(Int, Int)]] =
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

   def find(grid: List[String], dirs: List[String], rng: Range, lin: Int, pos: Int): List[IndexedSeq[Char]] =
      dirs.map { dir =>
         val v = star(pos, lin, rng).getOrElse(dir, Nil)
         for i <- v.indices
         yield Try(grid(v(i)._2)(v(i)._1)) match
            case Failure(_)     => ' '
            case Success(value) => value
      }

}
