package kernel.models

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.stats.distributions.Dirichlet

class RandomNonDeterministicHMMModel(n: Int, numTest: Int = 10) extends DeterministicHMMModel(n, numTest) {

  val diric = new Dirichlet(DenseVector.ones[Double](3) * 0.5)
  var _transMatrix: DenseMatrix[Double] = _
  var _emMatrix: DenseMatrix[Double] = _

  override protected def hiddenStates = Seq("A", "B", "C")

  override protected def visibleStates = Seq("X", "Y", "Z")

  override protected def transitionMatrix: DenseMatrix[Double] = {
    if (_transMatrix == null)
      _transMatrix = DenseMatrix(
        draw(),
        draw(),
        draw()
      )

    _transMatrix
  }

  override protected def emissionMatrix: DenseMatrix[Double] = {
    if (_emMatrix == null)
      _emMatrix = DenseMatrix(
        draw(),
        draw(),
        draw()
      )

    _emMatrix
  }

  private def draw(): Array[Double] = {
    val vec: DenseVector[Double] = diric.draw()
    vec.toArray
  }

  override def refresh() = {
    _transMatrix = null
    _emMatrix = null
  }
}
