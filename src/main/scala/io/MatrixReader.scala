package io

import breeze.linalg.DenseMatrix
import scala.io.Source

object MatrixReader {

  def loadMatrixFromFile(filepath: String): DenseMatrix[Double] = {
    var data = Seq[Array[Double]]()
    Source.fromFile(filepath).getLines().foreach(line => {
      val row = line.split(",").map(_.toDouble)
      data :+= row
    })

    if (data.length > 0)
      DenseMatrix(data: _*)
    else
      null
  }

}
