package kernel.models

import breeze.linalg.{DenseMatrix, DenseVector}

trait ParsedModel {
  def generateTestData(): Array[DenseMatrix[Double]]
  def getTestLabels: Array[Array[String]]
}
