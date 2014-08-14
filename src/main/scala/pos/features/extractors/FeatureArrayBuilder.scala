package pos.features.extractors

import breeze.linalg.{SparseVector, any, DenseVector}
import computation.FeatureVector

object FeatureArrayBuilder {
  /**
   * Take a load of feature vector objects and convert them to actual feature vectors
   */
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

  private def parseFeatureVectors(featureVectors: Seq[Seq[FeatureVector]], index: Map[String, Int]): Array[Array[SparseVector[Double]]] = {
    var output = Seq[Array[SparseVector[Double]]]()

    featureVectors.foreach(sentence => {
      val featureSeq: Seq[SparseVector[Double]] = sentence.map(vec => parseFeatureVector(vec, index))
      assert(featureSeq.slice(1, sentence.length).forall(vec => any(vec)), "Should have at least one non-zero entry")
      output :+= featureSeq.toArray
    })

    output.toArray
  }

  private def parseFeatureVector(featureVector: FeatureVector, index: Map[String, Int]): SparseVector[Double] = {
    val arr = SparseVector.zeros[Double](index.size)

    featureVector.keys.foreach(key => {
      val position = index(key)
      arr(position) = featureVector(key)
    })

    arr
  }

}
