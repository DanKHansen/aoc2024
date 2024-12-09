@main
def day7(): Unit = {
   val src = getSource("7.txt")
   val testCases = src.map { case s"$result: $xs" => (result.toLong, xs.split(" ").map(_.toLong).toList) }

   def check(value: Long, xs: List[Long], concatenation: Boolean = false): Boolean =
      xs match
         case ::(head, Nil) => head == value
         case first :: second :: rest =>
            check(value, (first * second) :: rest, concatenation) || check(
              value,
              (first + second) :: rest,
              concatenation) ||
            (concatenation && check(value, (first.toString ++ second.toString).toLong :: rest, concatenation))
         case Nil => false

   println(s"1: ${testCases.map((testCase, xs) => if check(testCase, xs) then testCase else 0).sum}")
   println(s"2: ${testCases.map((testCase, xs) => if check(testCase, xs, true) then testCase else 0).sum}")
}
