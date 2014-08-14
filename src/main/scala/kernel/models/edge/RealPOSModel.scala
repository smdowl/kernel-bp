package kernel.models.edge

import app.Constants
import breeze.linalg.{SparseVector, DenseVector}
import input.ConllParser
import pos.features.extractors.BasicFeatureExtractor
import pos.output.ParsedFeaturesOutput

class RealPOSModel extends HMMModel {
  override protected def generateFeatureVectors(): (Array[String], Array[Array[SparseVector[Double]]], Array[Array[SparseVector[Double]]]) = {
    val parser = new ConllParser()
    val extractor = new BasicFeatureExtractor

    ParsedFeaturesOutput(parser, extractor, -1, Constants.MINI_TEST_FILE, Constants.MINI_TEST_FILE)
  }
}