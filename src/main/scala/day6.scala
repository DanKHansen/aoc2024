import scala.annotation.tailrec
import scala.concurrent.{Future, Await}

import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global
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
      p._1 < 0 || p._1 >= g.length || p._2 < 0 || p._2 >= g.head.length

   def isNextStepClear(p: Position, g: List[String] = grid): Boolean =
      g(p._1)(p._2) != '.'

   def turnRight(d: Direction): Direction = d match
      case Direction.North => Direction.East
      case Direction.South => Direction.West
      case Direction.East  => Direction.South
      case Direction.West  => Direction.North

   def isLoop(m: Map[Position, Int]): Boolean =
      // hardcoded value of crossings in original path (4) ...
      m.values.exists(_ > 4)

   def modGrid(p: Position) = grid.updated(p._1, grid(p._1).updated(p._2, 'X'))

   @tailrec
   def walk(
       curDir: Direction,
       curPos: Position,
       visited: Map[Position, Int],
       g: List[String] = grid
   ): (Map[Position, Int], Boolean) = {
      val (y, x) = curPos
      val nextStep = curDir match
         case Direction.North => (y - 1, x)
         case Direction.South => (y + 1, x)
         case Direction.East  => (y, x + 1)
         case Direction.West  => (y, x - 1)
      if isLoop(visited) then (visited, true)
      else if isPosOutOfBounds(nextStep, g) then (visited.updated(curPos, visited.getOrElse(curPos, 0) + 1), false)
      else if isNextStepClear(nextStep, g) then walk(turnRight(curDir), curPos, visited, g)
      else walk(curDir, nextStep, visited.updated(curPos, visited.getOrElse(curPos, 0) + 1), g)

   }

   val (visitedPos, _) = walk(Direction.North, startPos, Map(startPos -> 0))

   val crossings = visitedPos.count(_._2 >= 2)

   val futures = visitedPos.keySet
      .filterNot(_ == startPos)
      .map { blockPos =>
         Future {
            (blockPos, walk(Direction.North, startPos, Map(startPos -> 0), modGrid(blockPos)))
         }
      }

   val results = Future.sequence(futures)
   val finalResults = Await.result(results, 60.seconds)
   val loops = finalResults.count(x => x._2._2)

   println(s"1: ${(visitedPos.size, crossings)}")
   println(s"2: $loops")

}
