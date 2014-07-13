package pos.features.extractors

import breeze.linalg.{any, DenseVector}
import computation.FeatureVector

object FeatureArrayBuilder {
  def buildFeatureArray(trainFeatureVectors: Seq[Seq[FeatureVector]],
                        testFeatureVectors: Seq[Seq[FeatureVector]]) = {

    val keyArray = getKeyArray(trainFeatureVectors ++ testFeatureVectors)
    val featureIndex = buildInverseIndex(keyArray)

    val trainOut = parseFeatureVectors(trainFeatureVectors, featureIndex)
    val testOut = parseFeatureVectors(testFeatureVectors, featureIndex)

    (keyArray, trainOut, testOut)
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

  private def parseFeatureVectors(featureVectors: Seq[Seq[FeatureVector]], index: Map[String, Int]): Array[Array[DenseVector[Double]]] = {
    var output = Seq[Array[DenseVector[Double]]]()

    featureVectors.foreach(sentence => {
      val featureSeq: Seq[DenseVector[Double]] = sentence.map(vec => parseFeatureVector(vec, index))
      assert(featureSeq.slice(1, sentence.length).forall(vec => any(vec)), "Should have at least one non-zero entry")
      output :+= featureSeq.toArray
    })

    output.toArray
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
