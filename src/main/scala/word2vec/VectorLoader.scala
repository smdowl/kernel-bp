package word2vec

import scala.io.Source
import breeze.linalg._

class VectorLoader {
  private val mapPath = "/Users/shaundowling/dev/word2vec/out-file-2"

  def findVector(search: String): Vector[Double] = {
    for (line <- Source.fromFile(mapPath).getLines()) {

      val split = line.split('\t')

      val word = split(0)

      if (word.equals(search)) {
        val vector = parseVector(split(1))
        println(word)
        return vector
      }
    }

    throw new Exception("No Vector found")
  }

  private def parseVector(vecString: String) = {
    val vec: Seq[String] = vecString.substring(1, vecString.length-1).split(',')
    val doubleVec = vec.map(parseDouble)
    DenseVector(doubleVec:_*)
  }

  private def parseDouble(string: String) = try { string.toDouble } catch { case _: Throwable => 0.0 }
}
