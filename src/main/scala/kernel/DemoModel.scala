package kernel

import breeze.linalg._

object DemoModel extends App {
  val model = new DemoModel(10)
  println(model.generateData())
}

class DemoModel(n: Int) extends Model(n) {
  override def A: DenseMatrix[Int] = DenseMatrix(
    (0,1,1,0,0),
    (0,0,0,1,1),
    (0,0,0,0,0),
    (0,0,0,0,0),
    (0,0,0,0,0) )

  // Model parameters
  val rootMeans = DenseVector(0,2)
  val rootStd = 0.2
  val p1Root = 0.5

  val middleStd = 0.2

  val leafStd = 0.2
  val p1Leaf = 0.5

  val sampleOrder = 0 until numNodes

  def generateData(): DenseMatrix[Double] = {

    // Sampling step
    val outputArray: DenseMatrix[Double] = DenseMatrix.zeros[Double](n, numNodes)

    for (sampleInd <- 0 until n) {
      generateSample(outputArray, sampleInd)
    }

    outputArray
  }

  private def generateSample(outputArray: DenseMatrix[Double], sampleInd: Int) = {
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
}
