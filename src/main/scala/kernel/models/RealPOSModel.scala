package kernel.models

import app.Constants
import breeze.linalg.SparseVector
import input.ConllParser
import pos.features.extractors.{POSFeatureExtractor, UnigramWordVecFeatureExtractor, UnigramPOSFeatureExtractor}
import pos.output.ParsedFeaturesOutput

class RealPOSModel(extractor: POSFeatureExtractor = new UnigramWordVecFeatureExtractor, same: Boolean=false) extends HMMModel {
  override protected def generateFeatureVectors(): (Array[String], Array[Array[SparseVector[Double]]], Array[Array[SparseVector[Double]]]) = {
    val parser = new ConllParser()

    val trainfile = Constants.MINI_TRAIN_FILE
    val testfile = if (same) trainfile else Constants.MINI_TEST_FILE
    ParsedFeaturesOutput(parser, extractor, -1, trainfile, testfile)
  }

  def getExtractor = extractor

  def getNumberOfSamples: Int = edges("visible").startData.rows

}