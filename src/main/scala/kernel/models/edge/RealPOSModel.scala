package kernel.models.edge

import app.Constants
import breeze.linalg.DenseVector
import input.ConllParser
import pos.features.extractors.BasicFeatureExtractor
import pos.output.ParsedFeaturesOutput

class RealPOSModel extends HMMModel {
  val parser = new ConllParser()
  val extractor = new BasicFeatureExtractor

  override protected def generateFeatureVectors(): (Array[String], Array[Array[DenseVector[Double]]], Array[Array[DenseVector[Double]]]) = {
    ParsedFeaturesOutput(parser, extractor, -1, Constants.MINI_TRAIN_FILE, Constants.MINI_TEST_FILE)
  }
}
