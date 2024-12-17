import scala.annotation.tailrec
import sources._
object day12 {
   def main(args: Array[String]): Unit = {
      type Region = Vector[(Int, Int)]

      def adjacent(x: Int, y: Int): List[(Int, Int)] =
         List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))

      def neighbors(ix: Int, iy: Int): List[(Int, Int)] =
         for {
            x <- (ix - 1 to ix + 1).toList
            y <- (iy - 1 to iy + 1).toList
            if x != ix || y != iy
         } yield (x, y)

      case class PlantMap(plants: Vector[String]) {
         val height: Int = plants.size
         val width: Int = plants.head.length

         def apply(x: Int, y: Int): Char = plants(y)(x)

         def isDefinedAt(x: Int, y: Int): Boolean = x >= 0 && x < width && y >= 0 && y < height

         def get(x: Int, y: Int): Option[Char] = Option.when(isDefinedAt(x, y))(apply(x, y))

         def indices: Vector[(Int, Int)] =
            for {
               x <- (0 until width).toVector
               y <- (0 until height).toVector
            } yield (x, y)

         def optionalNeighbors(x: Int, y: Int): List[Option[Char]] = neighbors(x, y).map(get)

         def floodFill(x: Int, y: Int): Region =
            val char = apply(x, y)

            @tailrec
            def loop(queue: List[(Int, Int)], visited: Set[(Int, Int)]): Set[(Int, Int)] =
               queue match {
                  case Nil => visited
                  case head :: tail if !get(head._1, head._2).contains(char) || visited.contains(head) =>
                     loop(tail, visited)
                  case head :: tail =>
                     val newVisited = visited + head
                     val newQueue = tail ++ adjacent(head._1, head._2)
                     loop(newQueue, newVisited)
               }

            loop(List((x, y)), Set.empty).toVector

         def optionalAdjacent(x: Int, y: Int): List[Option[Char]] = adjacent(x, y).map(get)

         def regions: List[Region] =
            List.unfold(this.indices): acc =>
               acc.headOption.map: head =>
                  val points = floodFill(head._1, head._2)
                  (points, acc.diff(points))
      }

      extension (region: Region) {
         private def asPlantMap: PlantMap =
            val maxX = region.map(_._1).max
            val maxY = region.map(_._2).max
            val regionSet = region.toSet
            val res = (0 to maxY).toVector.map: y =>
               (0 to maxX).toVector
                  .map: x =>
                     if (regionSet.contains((x, y))) '#' else '.'
                  .mkString
            PlantMap(res)
         private def area: Int = region.size
         private def perimeter: Int =
            val regionMap = region.asPlantMap
            region.map((x, y) => regionMap.optionalAdjacent(x, y).count(_.forall(_ != '#'))).sum
         private def inflate: Region =
            for {
               (x, y) <- region
               dx <- 0 to 1
               dy <- 0 to 1
            } yield (x * 2 + dx, y * 2 + dy)
         private def sides: Int =
            val bigRegion = region.inflate
            val regionMap = bigRegion.asPlantMap
            bigRegion.count: (x, y) =>
               val count = regionMap.optionalNeighbors(x, y).count(_.contains('#'))
               count match {
                  case 3 | 4 | 7 => true
                  case _         => false
               }
      }

      val map = PlantMap(getSource("12.txt").toVector)

      println(s"1: ${map.regions.map(r => r.area * r.perimeter).sum}")
      println(s"2: ${map.regions.map(r => r.area * r.sides).sum}")
   }
}
