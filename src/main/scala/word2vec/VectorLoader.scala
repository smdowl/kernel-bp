package word2vec

import breeze.linalg._
import com.mongodb.casbah.Imports._

object VectorLoader extends App {
  val loader = new VectorLoader
  val res = loader.findVector("of")
  println(res)
}

class VectorLoader {
  val MONGO_DB = "word2vec"
  val MONGO_COLL = "vectors"

  def findVector(queryString: String): Vector[Double] = {
    val mongoConn = MongoConnection()
    val vectorsColl = mongoConn(MONGO_DB)(MONGO_COLL)

    val query = MongoDBObject("word" -> queryString)
    val result = vectorsColl.findOne(query)

    if (result.isDefined) {
      val vectorString = result.get("vector").asInstanceOf[String]
      return parseVector(vectorString)
    }

    println("Couldn't find " + queryString)
    returnErrorVector()
  }

  private def parseVector(vecString: String) = {
    val vec: Seq[String] = vecString.substring(1, vecString.length-1).split(',')
    val doubleVec = vec.map(parseDouble)
    DenseVector(doubleVec:_*)
  }

  private def parseDouble(string: String) = try { string.toDouble } catch { case _: Throwable => 0.0 }

  private def returnErrorVector(): Vector[Double] = {
    DenseVector.zeros(300)
  }
}
