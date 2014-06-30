package kernel.models

import breeze.linalg.DenseMatrix

class LoopyDemoModel(n: Int, datafile: String = "") extends DemoModel(n, datafile) {
  var __A: DenseMatrix[Int] = _

  override def _A: DenseMatrix[Int] = {
    if (__A == null)
      __A = DenseMatrix(
        (0, 1, 1, 0, 0),
        (1, 0, 0, 1, 1),
        (1, 1, 0, 1, 1),
        (0, 1, 1, 0, 0),
        (0, 0, 1, 0, 0))

    __A
  }
}
