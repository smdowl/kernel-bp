package kernel.models

import breeze.linalg._

abstract class Model(val n: Int) {

  /**
   * The directed adjacency matrix for this model.
   */
  def A: DenseMatrix[Int] = temp.getOrElse(_A)
  def numNodes: Int = A.rows

  /**
   * Allows for temporary changing the adjacency matrix (required for loopy BP)
   */
  var temp: Option[DenseMatrix[Int]] = Option.empty

  /**
   * An function that allows subclasses to return different adjacency matrices since overriding A didn't allow
   * for changing A throughout the algorithm (which is necessary for the loopy models).
   */
  protected def _A: DenseMatrix[Int]

  /**
   * Other model parameters that need to be set.
   */
  val msgParam: MessageParam
  val rootNode: Int

  private def isLeaf(nodeId: Int) = getChildren(nodeId).length == 0
  private def hasParents(nodeId: Int) = getParents(nodeId).length > 0

  def getNeighbours(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = (getChildren(nodeId).toSet ++ getParents(nodeId)).toSeq.sorted
  def getParents(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = A(::, nodeId).findAll(_ > 0)
  def getChildren(nodeId: Int, A: DenseMatrix[Int] = this.A): Seq[Int] = try {A(nodeId, ::).t.findAll(_ > 0)} catch {case _: Throwable => Seq[Int]()}

  /**
   * Generate data for the model. The output format is an array where each position is the training data
   * relevant to a given node and each row of those matrices is a single sample.
   */
  def generateData(): Array[DenseMatrix[Double]]

  /**
   * Return the pruned tree required for the tree based algorithm.
   */
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

  private def shouldCut(observedNodes: Set[Int], nodeId: Int): Boolean = {
    !observedNodes.contains(nodeId) && isLeaf(nodeId) && hasParents(nodeId)
  }
}
