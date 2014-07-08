package pos.features.extractors

import breeze.linalg.{DenseMatrix, DenseVector}
import computation.FeatureVector

object FeatureArrayBuilder {
  def buildFeatureArray(featureVectors: Array[FeatureVector]): Array[DenseMatrix[Double]] = {
    val keyArray = getKeyArray(featureVectors)
    val featureIndex = buildInverseIndex(keyArray)

    val featureSeq: Seq[DenseMatrix[Double]] = featureVectors.map(vec => buildFeatureArray(vec, featureIndex))

    featureSeq.toArray
  }

  private def getKeyArray(featureVectors: Array[FeatureVector]): Array[String] = {
    val fullSet = featureVectors.foldLeft(Set[String]())((set, vec) => set ++ vec.keySet)
    fullSet.toArray.sorted
  }

  private def buildInverseIndex(keyArray: Array[String]): Map[String, Int] = {
    val keyPairs = for (i <- 0 until keyArray.length) yield keyArray(i) -> i
    Map[String, Int]() ++ keyPairs
  }

  private def buildFeatureArray(featureVector: FeatureVector, index: Map[String, Int]): DenseMatrix[Double] = {
    val arr = DenseMatrix.zeros[Double](index.size, 1)

    featureVector.keys.foreach(key => {
      val position = index(key)
      arr(position, 1) = featureVector(key)
    })

    arr
  }

}
