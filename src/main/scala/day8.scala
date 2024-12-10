import scala.annotation.targetName

@main
def day8(): Unit = {
   val src = getSource("8.txt")
   val grid = src.map(_.toList).filter(_.nonEmpty)
   val maxLines = grid.size
   val maxPos = grid.head.size

   case class Antenna(freq: Char, pos: Pos)

   case class Pos(pos: Int, line: Int) {
      @targetName("add")
      def +(d: Distance): Pos =
         Pos(pos + d.deltaX, line + d.deltaY)
      def DistanceTo(other: Pos): Distance =
         Distance(pos - other.pos, line - other.line)
   }

   case class Distance(deltaX: Int, deltaY: Int) {
      def negate: Distance = Distance(-deltaX, -deltaY)
      def double: Distance = multiply(2)
      def multiply(n: Int): Distance = Distance(deltaX * n, deltaY * n)
   }

   case class Bounds(ll: Pos, ru: Pos) {
      def isWithin(pos: Pos): Boolean =
         pos.pos >= ll.pos && pos.pos <= ru.pos &&
            pos.line >= ll.line && pos.line <= ru.line
   }

   def isAntenna(c: Char): Boolean = c.isLetterOrDigit

   def antennas(a: Antenna, as: List[Antenna]): List[Antenna] =
      as.filter(_.freq == a.freq)

   def isPair(a1: Antenna, a2: Antenna): Boolean =
      a1.freq == a2.freq && a1.pos != a2.pos

   def antiOne(a1: Antenna, a2: Antenna): Option[Pos] =
      if (isPair(a1, a2)) {
         val d = a2.pos.DistanceTo(a1.pos)
         Some(a2.pos + d.negate.double)
      } else {
         None
      }

   def antiOneBounds(a1: Antenna, a2: Antenna, bs: Bounds): Set[Pos] =
      if (isPair(a1, a2)) {
         val d = a2.pos.DistanceTo(a1.pos).negate
         LazyList.iterate(a2.pos)(_ + d).takeWhile(bs.isWithin).toSet
      } else {
         Set.empty
      }

   def getAntinodes(as: List[Antenna], bs: Option[Bounds] = None): Set[Pos] =
      as.flatMap { a1 =>
         antennas(a1, as).flatMap { a2 =>
            bs match {
               case Some(b) => antiOneBounds(a1, a2, b)
               case None    => antiOne(a1, a2)
            }
         }
      }.toSet

   val as = grid.zipWithIndex.flatMap { case (cs, l) =>
      cs.zipWithIndex.flatMap { case (c, p) =>
         Option.when(isAntenna(c))(Antenna(c, Pos(p, l)))
      }
   }

   val ans = getAntinodes(as)
      .filter(p => p.pos >= 0 && p.pos < maxPos && p.line >= 0 && p.line < maxLines)

   val bounds = Bounds(Pos(0, 0), Pos(maxPos - 1, maxLines - 1))
   val ans2 = getAntinodes(as, Some(bounds))
      .filter(p => p.pos >= 0 && p.pos < maxPos && p.line >= 0 && p.line < maxLines)

   println(s"1: ${ans.size}")
   println(s"2: ${ans2.size}")
}
