package kernel.models

import breeze.linalg.{DenseMatrix, DenseVector}

trait PosModel {
  def generateTestData(): Array[DenseMatrix[Double]]
}
