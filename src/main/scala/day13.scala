
import sources._
object day13 {
   def main(args: Array[String]): Unit = {
      val src = getSource("13.txt").mkString("|")
      case class Claw(ax: Long, ay: Long, bx: Long, by: Long, x: Long, y: Long):
         def solve: Option[Long] = for
            b <- (x * ay - y * ax) safeDiv (bx * ay - by * ax)
            a <- (x - b * bx) safeDiv ax
         yield a * 3 + b

      object L:
         def unapply(s: String): Option[Long] = s.toLongOption

      object Claw:
         def parse(xs: Seq[String]): Option[Claw] =
            xs match
               case Seq(
                     s"Button A: X+${L(ax)}, Y+${L(ay)}",
                     s"Button B: X+${L(bx)}, Y+${L(by)}",
                     s"Prize: X=${L(x)}, Y=${L(y)}"
                   ) =>
                  Some(Claw(ax, ay, bx, by, x, y))
               case _ => None

      extension (a: Long)
         private infix def safeDiv(b: Long): Option[Long] =
            Option.when(b != 0 && a % b == 0)(a / b)

      def parse(input: String) = input.split("\\|+").toSeq.grouped(3).flatMap(Claw.parse).toSeq

      def solve(c: Claw) =
         for
            a <- 0 to 100
            b <- 0 to 100
            if a * c.ax + b * c.bx == c.x
            if a * c.ay + b * c.by == c.y
         yield a * 3L + b

      val diff = 10_000_000_000_000L

      println(s"1: ${parse(src).flatMap(solve).sum}")
      println(s"2: ${parse(src).map(c => c.copy(x = c.x + diff, y = c.y + diff)).flatMap(_.solve).sum}")
   }
}
