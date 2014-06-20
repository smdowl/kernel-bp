package kernel

import breeze.linalg._

class DemoModel(n: Int) extends Model(n) {
  override protected val A: DenseMatrix[Int] = DenseMatrix(
    (0,1,1,0,0),
    (0,0,0,1,1),
    (0,0,0,0,0),
    (0,0,0,0,0),
    (0,0,0,0,0) )

  def generateData(): DenseMatrix[Double] = {

    // Model parameters
    val rootMeans = DenseVector(0,2)
    val rootStd = 0.2
    val p1Root = 0.5

    val middleStd = 0.2

    val leafStd = 0.2
    val p1Leaf = 0.5

    val numNodes = A.rows

    val sampleOrder = 0 until numNodes

    // Sampling step
    val outputArray: DenseMatrix[Double] = DenseMatrix.zeros[Double](n, numNodes)

    for (sampleInd <- 0 until n) {
      for (nodeInd <- 0 until numNodes) {

        val whichNode = sampleOrder(nodeInd)
        val parentInd = getParents(whichNode)

        if (parentInd.length >= 1) { // Middle or leaf

          if (getChildren(whichNode).length >= 1) {
            val mu = outputArray(sampleInd, parentInd(0)) // Only one parent in this graph
            outputArray(sampleInd, whichNode) = mu + middleStd * randn()
          } else {
            val mu = outputArray(sampleInd, parentInd(0))
            val c = rand()
            if (c <= p1Leaf)
              outputArray(sampleInd, whichNode) = mu + leafStd * randn()
            else
              outputArray(sampleInd, whichNode) = 0 + leafStd * randn()
          }
        } else { // ROOT node
        val c = rand()
          if (c <= p1Root)
            outputArray(sampleInd, whichNode) = rootMeans(0) + rootStd * randn()
          else
            outputArray(sampleInd, whichNode) = rootMeans(1) + rootStd * randn()
        }
      }
    }

    outputArray
  }
}
