import sources.*

import scala.annotation.tailrec
object day15 {
   def main(args: Array[String]): Unit = {
      val src = getSource("15.txt")

      case class Pos(lin: Int, col: Int)

      enum Direction:
         case UP, DOWN, RIGHT, LEFT

      enum TileType:
         case WALL, FREE, BOX

      object TileType:
         def apply(c: Char): TileType = c match {
            case '#' => WALL
            case 'O' => BOX
            case _   => FREE
         }

      object Direction:
         val offsets: Map[Direction, Pos] = Map(
           UP -> Pos(-1, 0),
           DOWN -> Pos(1, 0),
           RIGHT -> Pos(0, 1),
           LEFT -> Pos(0, -1)
         )
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
      val startPos = initialWH.zipWithIndex.collectFirst {
         case (col, lin) if col.contains('@') => Pos(lin, col.indexOf('@'))
      }.get
      val wh = initialWH.updated(startPos.lin, initialWH(startPos.lin).updated(startPos.col, '.'))

      def tileType(p: Pos, wh: Vector[Vector[Char]]): TileType =
         TileType(wh(p.lin)(p.col))

      def moveFrom(currPos: Pos, direction: Direction): Pos =
         val offset = Direction.offsets(direction)
         Pos(currPos.lin + offset.lin, currPos.col + offset.col)

      @tailrec
      def go(currPos: Pos, steps: List[Direction], currWH: Vector[Vector[Char]]): Vector[Vector[Char]] =
         steps match {
            case Nil => currWH
            case dir :: tail =>
               val next = moveFrom(currPos, dir)
               tileType(next, currWH) match {
                  case TileType.WALL => go(currPos, tail, currWH)
                  case TileType.FREE => go(next, tail, currWH)
                  case TileType.BOX =>
                     val boxes = makeBoxList(currPos, dir, currWH)
                     if boxes.length > 1 then
                        val newWH = updateWH(boxes, currWH)
                        go(next, tail, newWH)
                     else go(currPos, tail, currWH)
               }
         }

      def makeBoxList(headPos: Pos, dir: Direction, wh: Vector[Vector[Char]]): List[Pos] = {
         @tailrec
         def loop(pos: Pos, acc: List[Pos]): List[Pos] = {
            val next = moveFrom(pos, dir)
            tileType(next, wh) match {
               case TileType.WALL => Nil
               case TileType.FREE => next :: acc
               case TileType.BOX  => loop(next, next :: acc)
            }
         }
         headPos :: loop(headPos, Nil).reverse
      }

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
