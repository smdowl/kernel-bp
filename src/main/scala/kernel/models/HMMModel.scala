package kernel.models

import breeze.linalg.DenseMatrix

abstract class HMMModel(n: Int, length: Int) extends Model(n) {
  override val rootNode: Int = 0
  private var cachedA: DenseMatrix[Int] = _

  override protected def _A: DenseMatrix[Int] = {
    if (cachedA == null) {
      cachedA = DenseMatrix.zeros[Int](2*length, 2*length)

      // Link the hidden component
      for (i <- 1 until length) {
        cachedA(i-1, i) = 1
      }

      for (i <- 0 until length)
        cachedA(i, length+i) = 1
    }

    cachedA
  }

  /**
   * Get the lists of node pairs that share the same distributional relationship.
   * Defaults to return each edge on it's own
   * @return
   */
  override def sharedSets: List[List[(Int, Int)]] = {
    var sets = List[List[(Int, Int)]]()

    sets :+= getHiddenSharedSets
    sets :+= getVisibleSharedSets

    sets
  }

  private def getHiddenSharedSets = (0 until length-1).foldLeft(List[(Int, Int)]())((b, a) => b :+ (a, a+1))
  private def getVisibleSharedSets = (0 until length).foldLeft(List[(Int, Int)]())((b, a) => b :+ (a, length+a))

}
