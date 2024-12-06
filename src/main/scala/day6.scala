import scala.annotation.tailrec

@main
def day6(): Unit = {
   type Position = (Int, Int)
   enum Direction:
      case North, South, East, West

   val src = getSource("6.txt")
   val turtle = src.find(_.contains('^')).get
   val lin_Idx = src.indexOf(turtle)
   val pos_Idx = turtle.indexOf('^')
   val startPos = (lin_Idx, pos_Idx)
   val max_X = src.head.indices.last
   val max_Y = src.indices.last
   val grid = src.updated(lin_Idx, src(lin_Idx).updated(pos_Idx, '.'))

   def isPosOutOfBounds(p: Position): Boolean =
      p._1 > max_Y || p._1 < 0 || p._2 > max_X || p._2 < 0

   def isNextStepClear(p: Position): Boolean =
      grid(p._1)(p._2) != '.'

   def turnRight(d: Direction): Direction = d match
      case Direction.North => Direction.East
      case Direction.South => Direction.West
      case Direction.East  => Direction.South
      case Direction.West  => Direction.North

   @tailrec
   def walk(curDir: Direction, curPos: Position, visited: Set[Position]): Int = {
      val (y, x) = curPos
      val nextStep = curDir match
         case Direction.North => (y - 1, x)
         case Direction.South => (y + 1, x)
         case Direction.East  => (y, x + 1)
         case Direction.West  => (y, x - 1)

      if (isPosOutOfBounds(nextStep)) visited.size + 1
      else if (isNextStepClear(nextStep))
         walk(turnRight(curDir), curPos, visited)
      else
         walk(curDir, nextStep, visited + curPos)
   }

   println(s"1: ${walk(Direction.North, startPos, Set.empty)}")
   // println(s"2: ${}")

}
