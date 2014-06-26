package kernel

import breeze.linalg._
import app.Constants
import io.MatrixReader

class DemoModel(n: Int, dataFile: String = "") extends Model(n) {
  override var A: DenseMatrix[Int] = DenseMatrix(
    (0,1,1,0,0),
    (0,0,0,1,1),
    (0,0,0,0,0),
    (0,0,0,0,0),
    (0,0,0,0,0) )

  override val msgParam = MessageParam(0.1, 0.3)

  // Model parameters
  override val rootNode = 0
  val rootMeans = DenseVector(0,2)
  val rootStd = 0.2
  val p1Root = 0.5

  val middleStd = 0.2

  val leafStd = 0.2
  val p1Leaf = 0.5

  val sampleOrder = 0 until numNodes

  var outputArray: DenseMatrix[Double] = _
  var sampleInd = 0

  override def generateData(): DenseMatrix[Double] = {

    if (shouldReadFromFile)
      readFromFile()
    else
      generateRandomData()

    outputArray
  }

  def shouldReadFromFile = !dataFile.equals("")

  def readFromFile() = {
    outputArray = MatrixReader.loadMatrixFromFile(dataFile)

    assert(outputArray.rows == n)
    assert(outputArray.cols == numNodes)
  }

  def generateRandomData() = {
    outputArray = DenseMatrix.zeros[Double](n, numNodes)

    sampleInd = 0
    while (sampleInd < n) {
      generateSample()
      sampleInd += 1
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
    if (getChildren(whichNode).length >= 1) {
      val mu = outputArray(sampleInd, parents(0)) // Only one parent in this graph
      outputArray(sampleInd, whichNode) = mu + middleStd * randn()
    } else {
      val mu = outputArray(sampleInd, parents(0))
      val c = rand()
      if (c <= p1Leaf)
        outputArray(sampleInd, whichNode) = mu + leafStd * randn()
      else
        outputArray(sampleInd, whichNode) = 0 + leafStd * randn()
    }
  }

  private def sampleRootNode(whichNode: Int) = {
    val c = rand()
    if (c <= p1Root)
      outputArray(sampleInd, whichNode) = rootMeans(0) + rootStd * randn()
    else
      outputArray(sampleInd, whichNode) = rootMeans(1) + rootStd * randn()
  }

  override def loadCorrect(): Array[DenseMatrix[Double]] = {
    var output = Seq[DenseMatrix[Double]]()

    for (i <- 1 to numNodes-1) {
      val filepath = Constants.CORRECT_DIR + s"betaArr$i"
      output :+= MatrixReader.loadMatrixFromFile(filepath)
    }

    output.toArray
  }
}
