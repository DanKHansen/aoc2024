import sources.*

object day15b:
   case class Pos(lin: Int, col: Int)

   def main(args: Array[String]): Unit =
      val (wh, steps) = getSource("15_2_smalltest.txt").span(_.nonEmpty)
      val wideWH = wh.map(_.map(c => if c == 'O' then "[]" else s"$c$c").mkString)
      val initPos = wideWH.zipWithIndex
         .collectFirst {
            case (col, lin) if col.contains('@') => Pos(lin, col.indexWhere(_ == '@'))
         }
         .getOrElse(Pos(0, 0))
      val initWH = wideWH.updated(initPos.lin, wideWH(initPos.lin).replace("@@", ".."))

      initWH foreach println

      println(initPos)
      println(steps.mkString.trim)
