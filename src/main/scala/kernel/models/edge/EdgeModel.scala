package kernel.models.edge

import breeze.linalg.DenseMatrix

trait EdgeModel {
  def edges: Map[String, Edge]
  def testObservations: Array[Map[Int, DenseMatrix[Double]]]
  def testLabels: Array[Map[Int, DenseMatrix[Double]]]
  def keyArray: Array[String]

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
}
