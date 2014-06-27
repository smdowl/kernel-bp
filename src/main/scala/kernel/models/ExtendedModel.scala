package kernel.models

import breeze.linalg._
import kernel.MessageParam

class ExtendedModel(n: Int) extends Model(n) {
  override var A: DenseMatrix[Int] = DenseMatrix(
    (0,1,1,0,0),
    (0,0,0,1,1),
    (0,0,0,0,0),
    (0,0,0,0,0),
    (0,0,0,0,0) )

  val d = 2

  override val msgParam = MessageParam(0.1, 0.3)

  // Model parameters
  override val rootNode = 0
  val rootMeans = DenseMatrix((0.0, 0.0), (2.0, 2.0))
  val rootStd = 0.2
  val p1Root = 0.5

  val middleStd = 0.2

  val leafStd = 0.2
  val p1Leaf = 0.5

  val sampleOrder = 0 until numNodes

  var outputArray: Array[DenseMatrix[Double]] = _
  var sampleCount = 0

  override def generateData(): Array[DenseMatrix[Double]] = {
    outputArray = Array.ofDim[DenseMatrix[Double]](numNodes)

    for (i <- 0 until numNodes)
      outputArray(i) = DenseMatrix.zeros[Double](n, d)

    sampleCount = 0
    while (sampleCount < n) {
      generateSample()
      sampleCount += 1
    }

    outputArray
  }

  private def generateSample() = {

    for (nodeInd <- 0 until numNodes) {

      val whichNode = sampleOrder(nodeInd)
      val parents = getParents(whichNode)

      // Middle or leaf node
      if (parents.length >= 1)
        sampleNonRootNode(whichNode, parents)

      // ROOT node
      else
        sampleRootNode(whichNode)
    }
  }

  private def sampleNonRootNode(whichNode: Int, parents: Seq[Int]) = {
    // Inner node
    if (getChildren(whichNode).length >= 1) {
      val mu = outputArray(parents(0))(sampleCount, ::) // Only one parent in this graph
      outputArray(whichNode)(sampleCount, ::) := mu + middleStd * randn()
    // Outer node
    } else {
      val mu = outputArray(parents(0))(sampleCount, ::)
      val c = rand()
      if (c <= p1Leaf)
        outputArray(whichNode)(sampleCount, ::) := mu + (randn(2).asInstanceOf[DenseVector[Double]] * leafStd).t
      else
        outputArray(whichNode)(sampleCount, ::) := (randn(2).asInstanceOf[DenseVector[Double]] * leafStd).t
    }
  }

  private def sampleRootNode(whichNode: Int) = {
    val c = rand()

    val randomComponent =  (randn(2).asInstanceOf[DenseVector[Double]] * rootStd).t

    val mean = if (c <= p1Root)
       rootMeans(0, ::)
    else
      rootMeans(1, ::)

    outputArray(whichNode)(sampleCount, ::) := mean + randomComponent
  }
}
