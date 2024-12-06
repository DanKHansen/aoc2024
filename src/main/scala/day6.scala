import scala.annotation.tailrec

@main
def day6(): Unit = {
   type Position = (Int, Int)
   enum Direction:
      case North, South, East, West

   val src = getSource("6.txt")
   val start: String = src.filter(_.contains('^')).mkString
   val linIndex = src.indexOf(start)
   val posIndex = start.indexOf('^')
   val startPos = (linIndex, posIndex)
   val maxPos = src.head.indices.last
   val maxLin = src.indices.last
   val grid = src.updated(linIndex, src(linIndex).updated(startPos._2, '.'))

   def isPosOutOfBounds(p: Position): Boolean =
      p._1 > maxLin || p._1 < 0 || p._2 > maxPos || p._2 < 0

   def isNextStepClear(p: Position): Boolean =
      grid(p._1)(p._2) != '.'

   def turnRight(d: Direction): Direction = d match
      case Direction.North => Direction.East
      case Direction.South => Direction.West
      case Direction.East  => Direction.South
      case Direction.West  => Direction.North

   @tailrec
   def walk(curDir: Direction, curPos: Position, visited: Set[Position]): Int = {
      val (lin, pos) = curPos
      val nextStep = curDir match
         case Direction.North => (lin - 1, pos)
         case Direction.South => (lin + 1, pos)
         case Direction.East  => (lin, pos + 1)
         case Direction.West  => (lin, pos - 1)

      if (isPosOutOfBounds(nextStep)) visited.size + 1
      else if (isNextStepClear(nextStep))
         walk(turnRight(curDir), curPos, visited)
      else
         walk(curDir, nextStep, visited + curPos)
   }

   println(s"1: ${walk(Direction.North, startPos, Set.empty)}")
   // println(s"2: ${}")

}
