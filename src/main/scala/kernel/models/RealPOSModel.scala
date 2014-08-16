package kernel.models

import app.Constants
import breeze.linalg.SparseVector
import input.ConllParser
import pos.features.extractors.BasicPOSFeatureExtractor
import pos.output.ParsedFeaturesOutput

class RealPOSModel extends HMMModel {
  override protected def generateFeatureVectors(): (Array[String], Array[Array[SparseVector[Double]]], Array[Array[SparseVector[Double]]]) = {
    val parser = new ConllParser()
    val extractor = new BasicPOSFeatureExtractor

    ParsedFeaturesOutput(parser, extractor, -1, Constants.MICRO_TRAIN_FILE, Constants.MINI_TEST_FILE)
  }
}