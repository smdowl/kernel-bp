package kernel

import breeze.linalg._

object DemoModel extends App {
  val model = new DemoModel(10)
  println(model.generateData())
}

class DemoModel(n: Int) extends Model(n) {
  override val A: DenseMatrix[Int] = DenseMatrix(
    (0,1,1,0,0),
    (0,0,0,1,1),
    (0,0,0,0,0),
    (0,0,0,0,0),
    (0,0,0,0,0) )

  override val msgParam = MessageParam(0.1, 0.3)

  // Model parameters
  val rootMeans = DenseVector(0,2)
  val rootStd = 0.2
  val p1Root = 0.5

  val middleStd = 0.2

  val leafStd = 0.2
  val p1Leaf = 0.5

  val sampleOrder = 0 until numNodes

  var outputArray: DenseMatrix[Double] = _
  var sampleInd = 0

  override def getPrunedTree(observedList: Iterable[Int]): DenseMatrix[Int] = {
    val prunedA = DenseMatrix.zeros[Int](A.rows, A.cols)
    prunedA += A
    var prunedNodes = Set[Int]()

    var numCuts = 1

    while (numCuts > 0) {
      numCuts = 0
      for (nodeId <- 0 until numNodes) {
        if (shouldCut(prunedNodes, nodeId)) {
          prunedA(::, nodeId) := DenseVector.zeros[Int](A.cols)
          prunedNodes += nodeId
          numCuts += 1
        }
      }
    }

    prunedA
  }

  private def shouldCut(prunedNodes: Set[Int], nodeId: Int): Boolean = {
    !prunedNodes.contains(nodeId) && isLeaf(nodeId) && hasParents(nodeId)
  }

  private def isLeaf(nodeId: Int) = getChildren(nodeId).length == 0
  private def hasParents(nodeId: Int) = getParents(nodeId).length > 0

  override def generateData(): DenseMatrix[Double] = {

    // Sampling step
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
}
