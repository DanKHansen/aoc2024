import scala.annotation.tailrec
@main
def day6(): Unit = {
   type Position = (Int, Int)
   enum Direction:
      case North, South, East, West

   val src = getSource("6_test.txt")
   val turtle = src.find(_.contains('^')).get
   val lin_Idx = src.indexOf(turtle)
   val pos_Idx = turtle.indexOf('^')
   val startPos = (lin_Idx, pos_Idx)
   // replace starting position with a '.'
   val grid = src.updated(lin_Idx, src(lin_Idx).updated(pos_Idx, '.'))

   def isPosOutOfBounds(p: Position, g: List[String] = grid): Boolean =
      p._1 > g.indices.last || p._1 < 0 || p._2 > g.head.indices.last || p._2 < 0

   def isNextStepClear(p: Position, g: List[String] = grid): Boolean =
      g(p._1)(p._2) != '.'

   def turnRight(d: Direction): Direction = d match
      case Direction.North => Direction.East
      case Direction.South => Direction.West
      case Direction.East  => Direction.South
      case Direction.West  => Direction.North

   def isLoop(l: List[Position]): Boolean =

      if l.length < 4 then false else {
         val x = l.take(2)
         val slope = (x.last._2 - x.head._2) / (x.last._1 - x.head._1)
         slope == 0}

   def modGrid = grid.updated(6, grid(6).updated(3, 'X')) // this should initiate a loop

   @tailrec
   def walk(
       curDir: Direction,
       curPos: Position,
       visited: List[Position],
       g: List[String] = grid,
       loopCounter: Int = 0): (Int, Int) = {
      val (y, x) = curPos
      val nextStep = curDir match
         case Direction.North => (y - 1, x)
         case Direction.South => (y + 1, x)
         case Direction.East  => (y, x + 1)
         case Direction.West  => (y, x - 1)

      if isPosOutOfBounds(nextStep, g) then (visited.toSet.size + 1, loopCounter)
      else if isNextStepClear(nextStep, g) then {
         println(visited)
         println(isLoop(visited))
         walk(turnRight(curDir), curPos, visited, g)}
      else walk(curDir, nextStep, curPos +: visited, g)

   }

   //println(s"1: ${walk(Direction.North, startPos, Nil)._1}")
   println(s"2: ${walk(Direction.North, startPos, Nil, modGrid)._2}")

}
