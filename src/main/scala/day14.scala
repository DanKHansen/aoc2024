import sources._
object day14 {
   def main(args: Array[String]): Unit = {
      val src = getSource("14.txt")
      val isTest: Boolean = src.size <= 12
      case class Size(height: Int, width: Int)
      given size : Size = if isTest then Size(7,11) else Size(103,101)

      enum Quadrant:
         case NW,NE,SW,SE

      case class Pos(x: Int, y: Int)
      case class Velocity(dx: Int, dy: Int)

      object I:
         def unapply(s: String): Option[Int] = s.toIntOption

      def parse(s: String): Robot =
         s match {
            case s"p=${I(x)},${I(y)} v=${I(dx)},${I(dy)}" => Robot(Pos(x, y), Velocity(dx, dy))
         }

      case class Robot(pos: Pos, v: Velocity)

      def quadrant(p: Pos)(using size: Size): Option[Quadrant] =
         ((p.x - size.width / 2).sign, (p.y - size.height / 2).sign) match
            case (1, 1) => Some(Quadrant.NW)
            case (-1, 1) => Some(Quadrant.NE)
            case (-1, -1) => Some(Quadrant.SE)
            case (1, -1) => Some(Quadrant.SW)
            case _ => None


      val robots = src.map(parse)

      def frame(robots: List[Robot], time: Int)(using size: Size): List[Pos] =
         robots.map {r =>
            val x = (((r.pos.x + r.v.dx * time) % size.width) + size.width) % size.width
            val y = (((r.pos.y + r.v.dy * time) % size.height) + size.height) % size.height
            Pos(x, y)
         }


      println(s"1: ${frame(robots, 100).flatMap(quadrant).groupBy(identity).map(_._2.length).product}")
      // println(s"2: ${}")
   }
}
