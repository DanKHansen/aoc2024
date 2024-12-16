@main
def day12(): Unit = {
   type Region = Vector[(Int, Int)]

   def adjacent(x: Int, y: Int): List[(Int, Int)] =
      List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))

   def neighbors(ix: Int, iy: Int): List[(Int, Int)] = {
      (ix - 1 to ix + 1).flatMap {x =>
         (iy - 1 to iy + 1).flatMap {y =>
            Option.when(x != ix || y != iy)((x, y))
         }
      }.toList
   }

   case class PlantMap(plants: Vector[String]) {
      val height: Int = plants.size
      val width: Int = plants.head.length
      def apply(x: Int, y: Int): Char = plants(y)(x)
      def isDefinedAt(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height
      def get(x: Int, y: Int): Option[Char] = Option.when(isDefinedAt(x, y))(apply(x, y))
      def indices: Vector[(Int, Int)] =
         (for
            x <- 0 until width
            y <- 0 until height
         yield (x, y)).toVector


      def optionalNeighbors(x: Int, y: Int): List[Option[Char]] = {
         neighbors(x, y).map(get)
      }
      def floodFill(x: Int, y: Int): Region =
         val q = scala.collection.mutable.Queue[(Int, Int)]()
         val char = apply(x, y)
         val res = scala.collection.mutable.ListBuffer[(Int, Int)]()
         q.addOne((x, y))
         while (q.nonEmpty)
            val n = q.removeHead()
            if (get(n._1, n._2).contains(char) && !res.contains(n))
               res.prepend(n)
               q.addAll(adjacent(n._1, n._2))
         res.toVector

      def optionalAdjacent(x: Int, y: Int): List[Option[Char]] = adjacent(x, y).map(get)
      def regions: List[Region] =
         List.unfold[Vector[(Int, Int)], Vector[(Int, Int)]](this.indices): acc =>
            acc.headOption.map: head =>
               val points = floodFill(head._1, head._2)
               (points, acc.diff(points))

   }

   extension (region: Region) {
      def asPlantMap: PlantMap =
         val maxX = region.maxBy(_._1)._1
         val maxY = region.maxBy(_._2)._2
         val res = scala.collection.mutable.ArrayBuffer.fill(maxY + 1, maxX + 1)('.')
         region.foreach((x, y) => res(y)(x) = '#')
         PlantMap(res.map(_.mkString("", "", "")).toVector)
      def area: Int = region.size
      def perimeter: Int =
         val regionMap = region.asPlantMap
         region.map((x, y) => regionMap.optionalAdjacent(x, y).count(_.forall(_ != '#'))).sum
      def inflate: Region = {
         region.flatMap((x, y) => List((x * 2, y * 2), (x * 2 + 1, y * 2), (x * 2, y * 2 + 1), (x * 2 + 1, y * 2 + 1)))
      }
      def sides: Int = {
         val bigRegion = region.inflate
         val regionMap = bigRegion.asPlantMap
         bigRegion.count { (x, y) =>
            val neighborCount = regionMap.optionalNeighbors(x, y).count(_.contains('#'))
            neighborCount match {
               case 3 | 4 | 7 => true
               case _ => false
            }
         }
      }
   }

   val map = PlantMap(getSource("12.txt").toVector)


   println(s"1: ${map.regions.map(r => r.area * r.perimeter).sum}")
   println(s"2: ${map.regions.map(r => r.area * r.sides).sum}")
}
