package kernel.models

import breeze.linalg._
import MessageParam

abstract class Model(val n: Int) {
  var temp: Option[DenseMatrix[Int]] = Option.empty
  val msgParam: MessageParam
  val rootNode: Int

  def numNodes: Int = A.rows

  def getPrunedTree(observedNodes: Set[Int]): (DenseMatrix[Int], Set[Int]) = {
    val prunedA = DenseMatrix.zeros[Int](A.rows, A.cols)
    prunedA += A

    temp = Option(prunedA)

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

    temp = Option.empty

    (prunedA, prunedNodes)
  }

  def A: DenseMatrix[Int] = temp.getOrElse(_A)
  protected def _A: DenseMatrix[Int]

  private def shouldCut(observedNodes: Set[Int], nodeId: Int): Boolean = {
    !observedNodes.contains(nodeId) && isLeaf(nodeId) && hasParents(nodeId)
  }

  private def isLeaf(nodeId: Int) = getChildren(nodeId).length == 0
  private def hasParents(nodeId: Int) = getParents(nodeId).length > 0

  def getNeighbours(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = (getChildren(nodeId).toSet ++ getParents(nodeId)).toSeq.sorted
  def getParents(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = A(::, nodeId).findAll(_ > 0)
  def getChildren(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = try {A(nodeId, ::).t.findAll(_ > 0)} catch {case _: Throwable => Seq[Int]()}

  def generateData(): Array[DenseMatrix[Double]]
}
