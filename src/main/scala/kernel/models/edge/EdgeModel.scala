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

  /**
   * The matrix of all possible label assignments.
   * i.e. each row is a vector of one possible labelling of a test node
   */
  def testMatrix: DenseMatrix[Double] = {
    val labels = keyArray.filter(_.startsWith("label:"))
    val labelIndex = this.keyIndex

    val matrix = DenseMatrix.zeros[Double](labels.length, keyArray.length)

    labels.zipWithIndex.foreach{ case (labelKey: String, idx: Int) => {
      matrix(idx, labelIndex(labelKey)) = 1.0
    }}

    matrix
  }
}
