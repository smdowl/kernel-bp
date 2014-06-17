package word2vec

import java.io.File
import scalax.io._
import breeze.linalg._

object VectorAppender extends App {
  val dataRoot = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/orig"
  val outputDir = "/Users/shaundowling/Google Drive/UCL/master project/code/history-gen/data/vector-appended"

  val dataFiles = new File(dataRoot).listFiles.filter(_.getName.endsWith(".conll"))
  val vectorLoader = new VectorLoader

  dataFiles.foreach(appendVectorsForFile)

  vectorLoader.close()

  private def appendVectorsForFile(file: File) {
    val output: Output = Resource.fromFile(new File(outputDir, file.getName))

    println(s"Starting ${file.getName}")

    scala.io.Source.fromFile(file).getLines().foreach(line => {
      val split = line.split('\t')

      val outString = if (split.length > 1) {
        val word = split(1)
        val vector = vectorLoader.findVector(word)
        line + '\t' + getVectorString(vector)
      } else {
        line
      }

      output.write(outString + '\n')
    })
  }

  private def getVectorString(vector: Vector[Double]) = vector.toArray.mkString(",")
}
