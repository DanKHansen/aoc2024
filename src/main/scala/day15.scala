import com.sun.nio.sctp.ShutdownNotification
import sources.*

import scala.annotation.tailrec
object day15 {
   def main(args: Array[String]): Unit = {
      val src = getSource("15.txt")

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

      @tailrec
      def go(currPos: Pos, steps: List[Direction], currWH: Vector[Vector[Char]]): Vector[Vector[Char]] =
         if steps.isEmpty then {
            // currWH.updated(currPos.lin, currWH(currPos.lin).updated(currPos.col, '@')) foreach println
            currWH
         } else
            val next = moveFrom(currPos, steps.head)
            next match {
               case p if isWall(p, currWH) => go(currPos, steps.tail, currWH)
               case p if isFree(p, currWH) => go(p, steps.tail, currWH)
               case p if isBox(p, currWH) =>
                  val boxes = makeBoxList(currPos, steps.head, currWH)
                  if boxes.length > 1 then
                     val newWH = updateWH(boxes, currWH)
                     go(p, steps.tail, newWH)
                  else go(currPos, steps.tail, currWH)
            }

      def makeBoxList(headPos: Pos, dir: Direction, wh: Vector[Vector[Char]]): List[Pos] =
         @tailrec
         def loop(pos: Pos, acc: List[Pos]): List[Pos] =
            val next = moveFrom(pos, dir)
            next match {
               case p if isWall(p, wh) => Nil
               case p if isFree(p, wh) => p :: acc
               case p if isBox(p, wh)  => loop(p, p :: acc)
            }
         headPos :: loop(headPos, Nil).reverse

      def updateWH(boxes: List[Pos], cWH: Vector[Vector[Char]]): Vector[Vector[Char]] =
         val (box1, box2) = (boxes.tail.head, boxes.last)
         val nwh = cWH.updated(box1.lin, cWH(box1.lin).updated(box1.col, '.'))
         nwh.updated(box2.lin, nwh(box2.lin).updated(box2.col, 'O'))

      def calculateGPS(wh: Vector[Vector[Char]]): Int = {
         for
            l <- wh.indices
            c <- wh(l).indices
         yield wh(l)(c) match
            case 'O' => Some(Pos(l, c))
            case _   => None
      }.flatten.map(p => 100 * p.lin + p.col).sum

      println(s"1: ${calculateGPS(go(startPos, steps, wh))}")
      // println(s"2: ${}")
   }
}
