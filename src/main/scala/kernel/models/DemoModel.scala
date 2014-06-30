package kernel.models

import app.Constants
import breeze.linalg._
import io.MatrixReader
import kernel.MessageParam

class DemoModel(n: Int, dataFile: String = "") extends Model(n) {
  override def _A: DenseMatrix[Int] = DenseMatrix(
    (0,1,1,0,0),
    (0,0,0,1,1),
    (0,0,0,0,0),
    (0,0,0,0,0),
    (0,0,0,0,0) )

  override val msgParam = MessageParam(0.1, 0.3)

  // Model parameters
  override val rootNode = 0
  val rootMeans = DenseMatrix(0.0, 2.0)
  val rootStd = 0.2
  val p1Root = 0.5

  val d = 1

  val middleStd = 0.2

  val leafStd = 0.2
  val p1Leaf = 0.5

  val sampleOrder = 0 until numNodes

  var outputArray: Array[DenseMatrix[Double]] = _
  var sampleCount = 0

  override def generateData(): Array[DenseMatrix[Double]] = {

    outputArray = Array.ofDim(numNodes)

    for (i <- 0 until numNodes)
      outputArray(i) = DenseMatrix.zeros[Double](n, d)

    if (shouldReadFromFile)
      readFromFile()
    else
      generateRandomData()

    outputArray
  }

  def shouldReadFromFile = !dataFile.equals("")

  def readFromFile() = {
    for (i <- 0 until numNodes)
      readDataFile(i)
  }
  
  def readDataFile(index: Int) = {
    outputArray(index) = MatrixReader.loadMatrixFromFile(dataFile + (index + 1))

    assert(outputArray(index).rows == n)
    assert(outputArray(index).cols == d)
  }

  def generateRandomData() = {
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
        outputArray(whichNode)(sampleCount, ::) := mu + leafStd * randn()
      else
        outputArray(whichNode)(sampleCount, ::) := 0 + leafStd * randn()
    }
  }

  private def sampleRootNode(whichNode: Int) = {
    val c = rand()

    val randomComponent =  rootStd * randn()

    val mean = if (c <= p1Root)
      rootMeans(0, ::)
    else
      rootMeans(1, ::)

    outputArray(whichNode)(sampleCount, ::) := mean + randomComponent
  }

  def loadCorrect(): Array[DenseMatrix[Double]] = {
    var output = Seq[DenseMatrix[Double]]()

    for (i <- 1 to numNodes-1) {
      val filepath = Constants.CORRECT_DIR + s"betaArr$i"
      output :+= MatrixReader.loadMatrixFromFile(filepath)
    }

    output.toArray
  }
}
