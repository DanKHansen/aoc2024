import scala.annotation.targetName

@main
def day10(): Unit = {
   val grid = getSource("10.txt").map(_.map(_.asDigit).toVector).toVector

   type Pos = (Int, Int)
   extension (pos: Pos)
      @targetName("add")
      def +(other: Pos): Pos = (pos(0) + other(0), pos(1) + other(1))

   type Grid = Vector[Vector[Int]]
   extension (grid: Grid)
      def apply(pos: Pos): Int =
         grid(pos(0))(pos(1))
      def onGrid(pos: Pos): Boolean =
         pos(0) >= 0 && pos(0) < grid.size &&
            pos(1) >= 0 && pos(1) < grid.head.size
      def positions: Seq[(Int, Int)] =
         for
            row <- grid.indices
            column <- grid.head.indices
         yield (row, column)

   type Graph = Map[Pos, Set[Pos]]

   def makeGraph(grid: Grid): Graph =
      def adjacent(pos: Pos): Set[Pos] =
         Set((-1, 0), (1, 0), (0, -1), (0, 1))
            .flatMap: offsets =>
               Some(pos + offsets)
                  .filter: nextPos =>
                     grid.onGrid(nextPos) && grid(nextPos) == grid(pos) + 1

      grid.positions
         .map(pos => pos -> adjacent(pos))
         .toMap

   def reachableSummits(pos: Pos, g: Graph): Set[Pos] =
      if grid(pos) == 9
      then Set(pos)
      else g(pos).flatMap(p => reachableSummits(p, g))

   def routes(pos: Pos, g: Graph): Int =
      if grid(pos) == 9
      then 1
      else g(pos).toSeq.map(p => routes(p, g)).sum

   val one = grid.positions.filter(p => grid(p) == 0).map(p => reachableSummits(p, makeGraph(grid)).size).sum
   val two = grid.positions.filter(pos => grid(pos) == 0).map(p => routes(p, makeGraph(grid))).sum

   println(s"1: $one")
   println(s"2: $two")

}
