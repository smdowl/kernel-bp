package kernel

import breeze.linalg._
import scala.io.Source
import app.Constants

object DemoModel extends App {
  val model = new DemoModel(10)
  println(model.generateData())

  val (pruned, _) = model.getPrunedTree(Set(3))
  assert(pruned.equals(DenseMatrix(
    (0,1,0,0,0),
    (0,0,0,1,0),
    (0,0,0,0,0),
    (0,0,0,0,0),
    (0,0,0,0,0)
  )), "Should match according to MATLAB implementation2")

  val readModel = new DemoModel(1000, "/Users/shaundowling/Google Drive/UCL/master project/code/kernelBP_source/kernelBP/sampArr")
  readModel.generateData()

  val correct = readModel.loadCorrect()
  println(correct)
}

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

  override def getPrunedTree(observedNodes: Set[Int]): (DenseMatrix[Int], Set[Int]) = {
    val prunedA = DenseMatrix.zeros[Int](A.rows, A.cols)
    prunedA += A
    val temp = A
    A = prunedA

    var prunedNodes = Set[Int]()

    var numCuts = 1

    while (numCuts > 0) {
      numCuts = 0
      for (nodeId <- 0 until numNodes) {
        if (shouldCut(observedNodes, nodeId)) {
          prunedA(::, nodeId) := 0
          prunedNodes += nodeId
          numCuts += 1
        }
      }
    }

    A = temp

    (prunedA, prunedNodes)
  }

  private def shouldCut(observedNodes: Set[Int], nodeId: Int): Boolean = {
    !observedNodes.contains(nodeId) && isLeaf(nodeId) && hasParents(nodeId)
  }

  private def isLeaf(nodeId: Int) = getChildren(nodeId).length == 0
  private def hasParents(nodeId: Int) = getParents(nodeId).length > 0

  override def generateData(): DenseMatrix[Double] = {

    if (shouldReadFromFile)
      readFromFile()
    else
      generateRandomData()

    outputArray
  }

  def shouldReadFromFile = !dataFile.equals("")

  def readFromFile() = {
    outputArray = loadMatrixFromFile(dataFile)

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
      output :+= loadMatrixFromFile(filepath)
    }

    output.toArray
  }

  private def loadMatrixFromFile(filepath: String): DenseMatrix[Double] = {
    var data = Seq[Array[Double]]()
    Source.fromFile(filepath).getLines().foreach(line => {
      val row = line.split(",").map(_.toDouble)
      data :+= row
    })

    if (data.length > 0)
      DenseMatrix(data: _*)
    else
      DenseMatrix.zeros[Double](0,0)
  }
}
