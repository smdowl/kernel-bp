package kernel.models

import breeze.linalg.DenseMatrix

class LoopyDemoModel(n: Int, datafile: String = "") extends LoopyModel(n, datafile) {
  override def unmoralisedA = DenseMatrix(
    (0, 1, 1, 0, 0),
    (0, 0, 0, 1, 0),
    (0, 0, 0, 1, 1),
    (0, 0, 0, 0, 0),
    (0, 0, 0, 0, 0))

  override def moralisedA = DenseMatrix(
    (0, 1, 1, 0, 0),
    (1, 0, 1, 1, 0),
    (1, 1, 0, 1, 1),
    (0, 1, 1, 0, 0),
    (0, 0, 1, 0, 0))
}
