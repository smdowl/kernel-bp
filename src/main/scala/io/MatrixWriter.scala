package io

import breeze.linalg.{DenseVector, DenseMatrix}
import sys.process._
import java.io.{PrintWriter, File}
import com.github.tototoshi.csv._

object MatrixWriter {
  def writeMatrixToFile(filepath: String, matrix: DenseMatrix[Double]) = {
//    val write = matrix.toString()
//    s"echo $write" #> new File(filepath) !

    val writer = CSVWriter.open(filepath)
    for (i <- 0 until matrix.rows) {
      val row: DenseVector[Double] = matrix(i, ::).t
      writer.writeRow(row.toArray)
    }

  }
}
