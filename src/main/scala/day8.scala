@main
def day8(): Unit = {
   val src = getSource("8.txt")
   class Record(elems: (String, Any)*) extends Selectable:
      private val fields = elems.toMap
      def selectDynamic(name: String): Any = fields(name)

   type Position = Record { val line: Int; val pos: Int }
   type Antenna = Record { val freq: Char; val pos: Position }

   val a: Antenna = Record("freq" -> 'A', "pos" -> Record("line" -> 4, "pos" -> 5)).asInstanceOf[Antenna]

   println(s"Frequency: ${a.selectDynamic("freq")}, Position: (${a.pos.selectDynamic("pos")}, ${a.pos.selectDynamic("line")})")


   // println(s"1: ${}")
   // println(s"2: ${}")
}
