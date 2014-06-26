package kernel.models

import breeze.linalg._
import kernel.MessageParam

abstract class Model(val n: Int) {
  var A: DenseMatrix[Int]
  val msgParam: MessageParam
  val rootNode: Int

  def numNodes: Int = A.rows

  def getPrunedTree(observedNodes: Set[Int]): (DenseMatrix[Int], Set[Int]) = {
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

  def getParents(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = A(::, nodeId).findAll(_ > 0)
  def getChildren(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = try {A(nodeId, ::).t.findAll(_ > 0)} catch {case _ => Seq[Int]()}

  def generateData(): DenseMatrix[Double]
}
