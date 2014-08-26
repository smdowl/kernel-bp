package kernel.models

import app.Constants
import breeze.linalg.SparseVector
import input.ConllParser
import pos.features.extractors.{POSFeatureExtractor, WordVecUnigramFeatureExtractor, UnigramPOSFeatureExtractor}
import pos.output.ParsedFeaturesOutput

class RealPOSModel(extractor: POSFeatureExtractor = new WordVecUnigramFeatureExtractor) extends HMMModel {
  override protected def generateFeatureVectors(): (Array[String], Array[Array[SparseVector[Double]]], Array[Array[SparseVector[Double]]]) = {
    val parser = new ConllParser()
    ParsedFeaturesOutput(parser, extractor, -1, Constants.MICRO_TRAIN_FILE, Constants.MICRO_TRAIN_FILE)
  }
}