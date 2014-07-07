package pos.features.extractors

import breeze.linalg.DenseVector
import computation.FeatureVector

object FeatureArrayBuilder {
  def buildFeatureArray(featureVectors: Array[FeatureVector]): Array[DenseVector[Double]] = {
    val keyArray = getKeyArray(featureVectors)
    val featureIndex = buildInverseIndex(keyArray)

    val featureSeq: Seq[DenseVector[Double]] = featureVectors.map(vec => buildFeatureArray(vec, featureIndex))

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

  private def buildFeatureArray(featureVector: FeatureVector, index: Map[String, Int]): DenseVector[Double] = {
    val arr = DenseVector.zeros[Double](index.size)

    featureVector.keys.foreach(key => {
      val position = index(key)
      arr(position) = featureVector(key)
    })

    arr
  }

}
