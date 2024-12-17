import sources._
import scala.annotation.tailrec
object day9 {
   def main(args: Array[String]): Unit = {
      type Disk = Seq[Option[Int]]
      type Disk2 = Seq[Block]

      enum Block(val size: Int):
         case Free(s: Int) extends Block(s)
         case File(s: Int, i: Int) extends Block(s)

         def index: Option[Int] = this match
            case Free(_)     => None
            case File(_, id) => Some(id)

         def canInsert(block: Block): Boolean = this match
            case Free(size) => size >= block.size
            case _          => false

      extension (free: Block.Free)
         def insert(b: Block): Seq[Block] =
            if b.size < free.size then Seq(b, Block.Free(free.size - b.size))
            else Seq(b)

      extension (disk: Disk)
         private def checksum: Long = disk.zipWithIndex
            .map(_.getOrElse(0).toLong * _)
            .sum

      extension (disk2: Disk2)
         private def checksum2: Long = disk2
            .flatMap(b => Vector.fill(b.size)(b.index.getOrElse(0)))
            .zipWithIndex
            .map(_.toLong * _)
            .sum

      def createDisk(input: String): Disk =
         input.toList
            .map(_.asDigit)
            .grouped(2)
            .toVector
            .zipWithIndex
            .flatMap:
               case (List(file, free), idx) => List.fill(file)(Some(idx)) ::: List.fill(free)(None)
               case (List(file), idx)       => List.fill(file)(Some(idx))
               case _                       => Nil

      def createDisk2(input: String): Disk2 =
         input.toList
            .map(_.asDigit)
            .grouped(2)
            .toVector
            .zipWithIndex
            .flatMap:
               case (List(file, free), idx) => Vector(Block.File(file, idx), Block.Free(free))
               case (List(file), idx)       => Vector(Block.File(file, idx))
               case _                       => Nil

      def moveBlocksForward(disk: Disk): Disk =
         @tailrec
         def loop(disk: Disk, acc: Disk, i: Int): Disk =
            if disk.size <= 1 then acc
            else
               disk.head match
                  case None           => loop(disk.last +: disk.tail.init, acc, i + 1)
                  case file @ Some(_) => loop(disk.tail, acc :+ file, i + 1)
         loop(disk, Vector.empty, 0)

      def moveFilesForward(disk: Disk2): Disk2 =
         @tailrec
         def loop(disk: Disk2, acc: Disk2): Disk2 =
            disk.lastOption match
               case None                       => acc
               case Some(last @ Block.Free(_)) => loop(disk.init, last +: acc)
               case Some(last @ Block.File(_, _)) =>
                  disk.zipWithIndex.find((block, _) => block.canInsert(last)) match
                     case None => loop(disk.init, last +: acc)
                     case Some(free @ Block.Free(_), id) =>
                        val newDisk = disk.take(id) ++ free.insert(last) ++ disk.drop(id + 1).init
                        loop(newDisk, Block.Free(last.size) +: acc)
                     case _ => Seq.empty
         loop(disk, Vector.empty)

      val metadata = getSource("9.txt").mkString
      println(s"1: ${moveBlocksForward(createDisk(metadata)).checksum}")
      println(s"2: ${moveFilesForward(createDisk2(metadata)).checksum2}")
   }
}
