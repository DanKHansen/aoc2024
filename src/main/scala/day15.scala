import com.sun.nio.sctp.ShutdownNotification
import sources.*

import scala.annotation.tailrec
object day15 {
   def main(args: Array[String]): Unit = {
      val src = getSource("15_test2.txt")

      case class Pos(lin: Int, col: Int)

      enum Direction:
         case UP, DOWN, RIGHT, LEFT

      enum Tiletype:
         case WALL, FREE, BOX

      object Tiletype:
         def apply(c: Char): Tiletype = c match {
            case '#' => WALL
            case 'O' => BOX
            case _   => FREE
         }

      object Direction:
         def apply(c: Char): Direction = c match {
            case '^' => UP
            case 'v' => DOWN
            case '>' => RIGHT
            case '<' => LEFT
         }

      def parse(in: List[String]) =
         val warehouse = in.takeWhile(_.nonEmpty)
         val attempts = in.drop(warehouse.size + 1)
         (warehouse.map(_.toVector).toVector, attempts.mkString.toList.map(Direction(_)))

      val (initialWH, steps) = parse(src)
      val startPos = {
         for {
            (line, lin) <- initialWH.zipWithIndex
            col <- line.indexOf('@') match {
               case -1 => None
               case c  => Some(c)
            }
         } yield Pos(lin, col)
      }.head

      val wh = initialWH.updated(startPos.lin, initialWH(startPos.lin).updated(startPos.col, '.'))

      def isWall(p: Pos, wh: Vector[Vector[Char]] = wh): Boolean =
         Tiletype(wh(p.lin)(p.col)) == Tiletype.WALL

      def isFree(p: Pos, wh: Vector[Vector[Char]] = wh): Boolean =
         Tiletype(wh(p.lin)(p.col)) == Tiletype.FREE

      def isBox(p: Pos, wh: Vector[Vector[Char]] = wh): Boolean =
         Tiletype(wh(p.lin)(p.col)) == Tiletype.BOX

      def moveFrom(currPos: Pos, direction: Direction): Pos =
         direction match
            case Direction.UP    => Pos(currPos.lin - 1, currPos.col)
            case Direction.DOWN  => Pos(currPos.lin + 1, currPos.col)
            case Direction.RIGHT => Pos(currPos.lin, currPos.col + 1)
            case Direction.LEFT  => Pos(currPos.lin, currPos.col - 1)

      // For each movement: check if the next step is either free, a wall or a box.
      //
      // In case of a wall, do nothing,
      // Start over with the next move
      // In case of a '.' Update the WH by switching the position of the robot and the free space.
      // Start over with the next move
      // In case of a box: Build a list of boxes in front of the robot
      //    check if the last box has a free space in front of it or not. (the previous 3 rules apply)
      //    Each boxlist consists of n number of boxes, and a last element that is either
      //    empty '.'
      //    or a wall '#'
      //    if last is a wall, do nothing
      // Start over with next move
      //    Otherwise 'move' the whole set of boxes one step forward,
      //    Update the warehouse to its new state:
      //    1. replace the last element with the first box,
      //    2. replace the first box with the robot, and finally
      //    3. replace the previous robot position with a '.'
      // Start over with the next move
      //
      // calculate the resulting GPS's, and sum them up.

      @tailrec
      def go(currPos: Pos, steps: List[Direction], currWH: Vector[Vector[Char]]): Vector[Vector[Char]] =
         if steps.isEmpty then currWH
         else
            moveFrom(currPos, steps.head) match {
               case p if isWall(p) => go(currPos, steps.tail, currWH)
               case p if isFree(p) => go(p, steps.tail, currWH)
               case p if isBox(p) =>
                  val boxes = makeBoxList(currPos, steps.head)
                  val newWH = updateWH(boxes, steps.head, currWH)
                  go(p, steps.tail, newWH)
            }

      def makeBoxList(headPos: Pos, dir: Direction): List[Pos] =
         @tailrec
         def loop(pos: Pos, acc: List[Pos]): List[Pos] =
            val next = moveFrom(pos, dir)
            next match {
               case p if isWall(p) => Nil
               case p if isFree(p) => p :: Nil
               case p if isBox(p)  => loop(next, pos :: acc)
            }
         headPos :: loop(headPos, Nil)

      def updateWH(boxes: List[Pos], dir: Direction, currWH: Vector[Vector[Char]]): Vector[Vector[Char]] =
         val nwh = currWH.updated(boxes.tail.head.lin, currWH(boxes.tail.head.lin).updated(boxes.tail.head.col, '.'))
         val nwh2 = nwh.updated(boxes.last.lin, nwh(boxes.last.lin).updated(boxes.last.col, 'O'))
         nwh2

      //def calculateGPS: List[Int] = ???

      go(startPos, steps, wh)

      // println(s"1: ${calculateGPS.sum}")
      // println(s"2: ${}")
   }
}
