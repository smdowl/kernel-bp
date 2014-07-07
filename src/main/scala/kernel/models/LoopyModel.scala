package kernel.models

import breeze.linalg.DenseMatrix

/**
 * A class repr a loopy model. Needs to do some messy stuff in order to handle creation of data
 * from an unmorlised graph, while performing inference on the original graph.
 */
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
