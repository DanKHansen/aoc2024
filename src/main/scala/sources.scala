import java.io.FileNotFoundException
import scala.util.{Failure, Success, Try}
object sources:
   def getSource(fileName: String): List[String] =
      val src = Try(scala.io.Source.fromResource(s"$fileName"))
      src match
         case Success(file) =>
            val input = file.getLines().toList
            file.close()
            input
         case Failure(e) => throw new FileNotFoundException(e.getMessage)
