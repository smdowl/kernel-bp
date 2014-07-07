package kernel.models

import breeze.linalg.DenseMatrix

abstract class LoopyModel(n: Int, datafile: String = "") extends DemoModel(n, datafile) {
  var storedA: DenseMatrix[Int] = _

  def unmoralisedA: DenseMatrix[Int]
  def moralisedA: DenseMatrix[Int]

  override def generateData(): Array[DenseMatrix[Double]] = {
    val temp = storedA
    storedA = unmoralisedA
    super.generateData()
    storedA = temp

    outputArray
  }

  override def _A: DenseMatrix[Int] = {
    if (storedA == null)
      storedA = moralisedA

    storedA
  }
}
