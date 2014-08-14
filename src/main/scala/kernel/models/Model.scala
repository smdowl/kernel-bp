package kernel.models

import breeze.linalg.{CSCMatrix, DenseMatrix, SparseVector}
import kernel.models.components.Edge

trait Model {
  protected var _edges: Map[String, Edge] = _
  var trainData: Array[Array[SparseVector[Double]]] = _
  protected var _testObservations: Array[Map[Int, CSCMatrix[Double]]] = _
  protected var _testLabels: Array[Map[Int, CSCMatrix[Double]]] = _
  protected var _keyArray: Array[String] = _

  def edges: Map[String, Edge] = _edges
  def testObservations: Array[Map[Int, CSCMatrix[Double]]] = _testObservations
  def testLabels: Array[Map[Int, CSCMatrix[Double]]] = _testLabels
  def keyArray: Array[String] = _keyArray

  def keyIndex: Map[String, Int] = {
    Map[String, Int]() ++ (0 until keyArray.length).map(i => {
      this.keyArray(i) -> i
    })
  }

  def labelKeys = keyArray.filter(_.startsWith("label:"))

  /**
   * The matrix of all possible label assignments.
   * i.e. each row is a vector of one possible labelling of a test node
   */
  def testMatrix: (Array[String], DenseMatrix[Double]) = {
    val labelIndex = this.keyIndex

    val matrix = DenseMatrix.zeros[Double](labelKeys.length, keyArray.length)

    labelKeys.zipWithIndex.foreach{ case (labelKey: String, idx: Int) => {
      matrix(idx, labelIndex(labelKey)) = 1.0
    }}

    (labelKeys, matrix)
  }

  protected def mergeData(dataSeq: Seq[DenseMatrix[Double]]): CSCMatrix[Double] = {
    val denseMat = if (dataSeq.length > 0)
      dataSeq.tail.foldLeft(dataSeq.head)((a, b) => DenseMatrix.vertcat(a, b))
    else
      DenseMatrix.zeros[Double](0, 0)

    kernel.linalg.toSparse(denseMat)
  }
}
