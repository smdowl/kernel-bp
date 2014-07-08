package pos.features.extractors

import breeze.linalg.DenseVector
import computation.FeatureVector

object FeatureArrayBuilder {
  def buildFeatureArray(featureVectors: Seq[Seq[FeatureVector]]) = {
    val keyArray = getKeyArray(featureVectors)
    val featureIndex = buildInverseIndex(keyArray)

    var output = Seq[Array[DenseVector[Double]]]()

    featureVectors.foreach(sentence => {
      val featureSeq: Seq[DenseVector[Double]] = sentence.map(vec => parseFeatureVector(vec, featureIndex))
      output :+= featureSeq.toArray
    })

    (keyArray, output)
  }

  private def getKeyArray(featureVectors: Seq[Seq[FeatureVector]]): Array[String] = {
    val allVectors = featureVectors.flatten
    val fullSet = allVectors.foldLeft(Set[String]())((set, vec) => set ++ vec.keySet)
    fullSet.toArray.sorted
  }

  private def buildInverseIndex(keyArray: Array[String]): Map[String, Int] = {
    val keyPairs = for (i <- 0 until keyArray.length) yield keyArray(i) -> i
    Map[String, Int]() ++ keyPairs
  }

  private def parseFeatureVector(featureVector: FeatureVector, index: Map[String, Int]): DenseVector[Double] = {
    val arr = DenseVector.zeros[Double](index.size)

    featureVector.keys.foreach(key => {
      val position = index(key)
      arr(position) = featureVector(key)
    })

    arr
  }

}
