package kernel.models

import app.Constants
import breeze.linalg._
import io.MatrixReader
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

  override def generateData(): DenseMatrix[Double] = {
    outputArray = Array.ofDim[DenseMatrix[Double]](n)

    sampleCount = 0
    while (sampleCount < n) {
      outputArray(sampleCount) = DenseMatrix.zeros[Double](numNodes, d)
      generateSample()
      sampleCount += 1
    }

    DenseMatrix.zeros[Double](n, numNodes)
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
      val mu = outputArray(sampleCount)(parents(0), ::) // Only one parent in this graph
      outputArray(sampleCount)(whichNode, ::) := mu + middleStd * randn()
    // Outer node
    } else {
      val mu = outputArray(sampleCount)(parents(0), ::)
      val c = rand()
      if (c <= p1Leaf)
        outputArray(sampleCount)(whichNode, ::) := mu + leafStd * randn()
      else
        outputArray(sampleCount)(whichNode, ::) := 0 + leafStd * randn()
    }
  }

  private def sampleRootNode(whichNode: Int) = {
    val c = rand()

    val randomComponent =  rootStd * randn()

    val mean = if (c <= p1Root)
       rootMeans(0, ::)
    else
      rootMeans(1, ::)

    outputArray(sampleCount)(whichNode, ::) := mean + randomComponent
  }
}
