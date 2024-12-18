import sources.*
import scala.annotation.tailrec
object day14 {
   def main(args: Array[String]): Unit = {
      val src = getSource("14.txt")
      val isTest: Boolean = src.size <= 12
      given dim: Dimension = if isTest then Dimension(7, 11) else Dimension(103, 101)

      case class Robot(pos: Pos, v: Velocity)
      case class Pos(x: Int, y: Int)
      case class Velocity(dx: Int, dy: Int)
      case class Dimension(height: Int, width: Int)
      type Robots = List[Pos]

      enum Dir:
         case N, S, E, W
      enum Quadrant:
         case NW, NE, SW, SE

      extension (p: Pos)
         private def vicinity(d: Dir): Pos = d match
            case Dir.N => Pos(p.x - 1, p.y)
            case Dir.S => Pos(p.x + 1, p.y)
            case Dir.E => Pos(p.x, p.y + 1)
            case Dir.W => Pos(p.x, p.y - 1)
         private def neighbors: List[Pos] = Dir.values.toList.map(p.vicinity)

      extension (robots: Robots)
         private def groups = robots.foldLeft(List.empty[Robots]): (groups, robot) =>
            val (newGroups, rest) = groups.partition: group =>
               robot.neighbors.exists(p => group.contains(p))
            (List(robot) +: newGroups).reduce(_ ++ _) +: rest
         private def christmas(groupSize: Int): Boolean = robots.groups.exists(_.size >= groupSize)

      object I:
         def unapply(s: String): Option[Int] = s.toIntOption

      def parse(s: String): Robot =
         s match {
            case s"p=${I(x)},${I(y)} v=${I(dx)},${I(dy)}" => Robot(Pos(x, y), Velocity(dx, dy))
         }

      val robots = src.map(parse)

      def quadrant(p: Pos)(using dim: Dimension): Option[Quadrant] =
         ((p.x - dim.width / 2).sign, (p.y - dim.height / 2).sign) match
            case (1, 1)   => Some(Quadrant.NW)
            case (-1, 1)  => Some(Quadrant.NE)
            case (-1, -1) => Some(Quadrant.SE)
            case (1, -1)  => Some(Quadrant.SW)
            case _        => None

      def frame(time: Int)(using dim: Dimension): List[Pos] =
         robots.map { r =>
            val x = (((r.pos.x + r.v.dx * time) % dim.width) + dim.width) % dim.width
            val y = (((r.pos.y + r.v.dy * time) % dim.height) + dim.height) % dim.height
            Pos(x, y)
         }

      @tailrec
      def findGroup(rng: List[Int]): Int =
         val rngRev = rng.reverse
         if rng.isEmpty || frame(rngRev.head).christmas(20) then rngRev.head else findGroup(rngRev.tail)

      def draw(t: Int)(using dim: Dimension): Unit =
         val pos = frame(t).toSet
         (0 until dim.height).foreach: y =>
            val l = (0 until dim.width).map(x => if pos(Pos(x, y)) then '*' else ' ').mkString
            println(l)
         println(s"Time: $t")
         println("-" * dim.width)

      val magicNumber = findGroup((0 to 8000).toList)

      println(s"1: ${frame(100).flatMap(quadrant).groupBy(identity).map(_._2.length).product}")
      println(s"2: $magicNumber")

      draw(magicNumber)
   }
}
