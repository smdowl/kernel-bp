package kernel.models

import breeze.linalg.DenseMatrix

class LoopyDemoModel(n: Int, datafile: String = "") extends DemoModel(n, datafile) {
  var __A: DenseMatrix[Int] = _
  val _A_orig = DenseMatrix(
    (0, 1, 1, 0, 0),
    (0, 0, 0, 1, 0),
    (0, 0, 0, 1, 1),
    (0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0))

  override def _A: DenseMatrix[Int] = {
    if (__A == null)
      __A = DenseMatrix(
        (0, 1, 1, 0, 0),
        (1, 0, 1, 1, 0),
        (1, 1, 0, 1, 1),
        (0, 1, 1, 0, 0),
        (0, 0, 1, 0, 0))

    __A
  }

  override def generateData(): Array[DenseMatrix[Double]] = {
    val temp = __A
    __A = _A_orig
    super.generateData()
    __A = temp

    outputArray
  }
}
