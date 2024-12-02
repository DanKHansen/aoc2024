@main
def day1(): Unit = {
   val data = getSource("1.txt") map (s => s.split(" " * 3).map(_.toInt))
   val left = Array.ofDim[Int](data.length)
   val right = Array.ofDim[Int](data.length)
   data.indices
      .foreach { i =>
         left(i) = data(i).head
         right(i) = data(i).last
      }
   val first = left.sorted.zip(right.sorted).map((x, y) => math.abs(x - y)).sum
   val second = data.indices.map(idx => right.count(_ == left(idx)) * left(idx)).sum

   println(first)
   println(second)
}
