import sources.*

import scala.annotation.tailrec
object day15:
   case class Pos(lin: Int, col: Int)
   
   def main(args: Array[String]): Unit =
      val src = getSource("15_2_smalltest.txt")

      enum Direction:
         case UP, DOWN, RIGHT, LEFT, NE, SE, SW, NW

      enum TileType:
         case WALL, FREE, BOX, ROBOT, LEFT_SIDE, RIGHT_SIDE

      object TileType:
         def apply(c: Char): TileType = c match
            case '#' => WALL
            case 'O' => BOX
            case '.' => FREE
            case '@' => ROBOT
            case '[' => LEFT_SIDE
            case ']' => RIGHT_SIDE

         def unapply(t: TileType): Option[Char] = t match
            case WALL       => Some('#')
            case BOX        => Some('O')
            case FREE       => Some('.')
            case ROBOT      => Some('@')
            case LEFT_SIDE  => Some('[')
            case RIGHT_SIDE => Some(']')

      object Direction:
         val offsets: Map[Direction, Pos] = Map(
           UP -> Pos(-1, 0),
           DOWN -> Pos(1, 0),
           RIGHT -> Pos(0, 1),
           LEFT -> Pos(0, -1),
           NE -> Pos(-1, 1),
           SE -> Pos(1, 1),
           SW -> Pos(1, -1),
           NW -> Pos(-1, -1)
         )
         def apply(c: Char): Direction = c match
            case '^' => UP
            case 'v' => DOWN
            case '>' => RIGHT
            case '<' => LEFT

      def parse1(in: List[String]) =
         val warehouse = in.takeWhile(_.nonEmpty).map(_.toVector).toVector
         val attempts = in.drop(warehouse.size + 1).mkString.toList.map(Direction(_))
         (warehouse, attempts)

      def parse2(in: List[String]) =
         val warehouse = in
            .takeWhile(_.nonEmpty)
            .map(s =>
               s.flatMap {
                  case '#' => Seq('#', '#')
                  case '.' => Seq('.', '.')
                  case 'O' => Seq('[', ']')
                  case '@' => Seq('@', '.')
               }.toVector)
            .toVector
         val attempts = in.drop(warehouse.size + 1).mkString.toList.map(Direction(_))
         (warehouse, attempts)

      val (initialWH, steps) = parse2(src)

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
         currWH.map(_.mkString) foreach println
         steps match
            case Nil => currWH
            case dir :: tail =>
               val next = moveFrom(currPos, dir)
               val boxes = makeBoxList(currPos, dir, currWH)
               (tileType(next, currWH), boxes.length > 1) match
                  case (TileType.FREE, _) => go(next, tail, currWH)
                  case (TileType.BOX, _) | (TileType.LEFT_SIDE, _) | (TileType.RIGHT_SIDE, _) =>
                     go(next, tail, updateWH(dir, boxes, currWH))
                  case _ => go(currPos, tail, currWH)

      def makeBoxList(headPos: Pos, dir: Direction, wh: Vector[Vector[Char]]): List[Pos] =
         @tailrec
         def loop(pos: Pos, acc: List[Pos]): List[Pos] =
            val next = moveFrom(pos, dir)
            tileType(next, wh) match
               case TileType.FREE => next :: acc
               case TileType.BOX | TileType.LEFT_SIDE | TileType.RIGHT_SIDE =>
                  loop(next, next :: acc)
               case _ => Nil
         headPos :: loop(headPos, Nil).reverse

      def updateWH(dir: Direction, boxes: List[Pos], cWH: Vector[Vector[Char]]): Vector[Vector[Char]] =
         @tailrec
         def updHoriz(l: List[Pos], wh: Vector[Vector[Char]]): Vector[Vector[Char]] =
            val c = if l.length % 2 == 0 then ']' else '['
            if l.isEmpty then wh
            else
               val uWH = wh.updated(l.head.lin, wh(l.head.lin).updated(l.head.col, c))
               updHoriz(l.tail, uWH)

         //@tailrec
         def updVert(l: List[Pos], wh: Vector[Vector[Char]]): Vector[Vector[Char]] =
            println(l)
            wh

         (dir, tileType(boxes.tail.head, cWH)) match
            case (_, TileType.BOX) =>
               val (box1, box2) = (boxes.tail.head, boxes.last)
               val nwh = cWH.updated(box1.lin, cWH(box1.lin).updated(box1.col, '.'))
               nwh.updated(box2.lin, nwh(box2.lin).updated(box2.col, 'O'))
            case (Direction.LEFT | Direction.RIGHT, TileType.RIGHT_SIDE) |
                (Direction.LEFT | Direction.RIGHT, TileType.LEFT_SIDE) =>
               val nwh = cWH.updated(boxes.tail.head.lin, cWH(boxes.tail.head.lin).updated(boxes.tail.head.col, '.'))
               updHoriz(boxes.drop(2), nwh)
            case (Direction.UP | Direction.DOWN, TileType.RIGHT_SIDE) |
                (Direction.UP | Direction.DOWN, TileType.LEFT_SIDE) =>
               updVert(boxes, cWH)
            case _ => cWH

      def calculateGPS(wh: Vector[Vector[Char]]): Int =
         wh.indices
            .flatMap(l => wh(l).indices.collect { case c if wh(l)(c) == 'O' => Pos(l, c) })
            .map(p => 100 * p.lin + p.col)
            .sum

      go(startPos, steps, wh)

      println(s"1: ${calculateGPS(go(startPos, steps, wh))}")
      // println(s"2: ${parse2(src)}")
