package kernel.models.toys

import breeze.linalg.DenseMatrix
import kernel.models.{HMMModel, ParsedModel, MessageParam}

object DeterministicHMMModel extends App {
  val model = new DeterministicHMMModel(10, 3)
  println(model.sharedSets)
}

class DeterministicHMMModel(n: Int, length: Int) extends HMMModel(n, length) with ParsedModel {

  override val msgParam: MessageParam = MessageParam(0.1, 0.3)



  /**
   * Generate data for the model. The output format is an array where each position is the training data
   * relevant to a given node and each row of those matrices is a single sample.
   */
  override def generateData(): Array[DenseMatrix[Double]] = ???

  override def generateTestData(): Array[DenseMatrix[Double]] = ???

  override def getTestLabels: Array[Array[String]] = ???

}

