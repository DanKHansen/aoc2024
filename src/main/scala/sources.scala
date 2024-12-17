import scala.util.{Failure, Success, Try}
object sources {
   def getSource(fileName: String): List[String] = {
      val cwd = System.getProperty("user.dir")
      val src = Try(
         scala.io.Source
            .fromFile(s"$cwd/sources/$fileName"))
      src match
         case Success(file) =>
            val input = file.getLines().toList
            file.close()
            input
         case Failure(_) => Nil
   }
}