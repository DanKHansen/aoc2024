import scala.util.{Failure, Success, Try}

@main
def day4(): Unit = {
   val grid = getSource("4.txt")
   val lines = grid.indices
   val positions = grid.head.indices

   println(s"1: ${countXmas(lines, positions)}")

   def countXmas(lin: Range, pos: Range): Int =
      (for
         l <- lin
         p <- pos
      yield findXmas(l, p).map(v => if (v.toList.mkString == "XMAS") true else false)).flatten.count(_ == true)

   def star(pos: Int, lin: Int): Map[String, Seq[(Int, Int)]] = Map(
     "n" -> (for i <- 0 until 4 yield (pos, lin - i)),
     "ne" -> (for i <- 0 until 4 yield (pos + i, lin - i)),
     "e" -> (for i <- 0 until 4 yield (pos + i, lin)),
     "se" -> (for i <- 0 until 4 yield (pos + i, lin + i)),
     "s" -> (for i <- 0 until 4 yield (pos, lin + i)),
     "sw" -> (for i <- 0 until 4 yield (pos - i, lin + i)),
     "w" -> (for i <- 0 until 4 yield (pos - i, lin)),
     "nw" -> (for i <- 0 until 4 yield (pos - i, lin - i))
   )

   def findXmas(l: Int, p: Int): List[IndexedSeq[Char]] =
      List("n", "ne", "e", "se", "s", "sw", "w", "nw").map { dir =>
         val v = star(l, p).getOrElse(dir, Nil)
         for i <- 0 until 4
         yield Try(grid(v(i)._2)(v(i)._1)) match
            case Failure(_)     => ' '
            case Success(value) => value
      }

}
