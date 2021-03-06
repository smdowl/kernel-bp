package kernel.models

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.Multinomial
import kernel.models.toyextractors.{UnigramFeatureExtractor, ToyFeatureExtractor}

class TrigramModel(n: Int, numTest: Int = 10) extends ProbabalisticHMMModel(n, numTest) {
  protected def hiddenStates = Seq("A", "B")
  protected def visibleStates = Seq("X", "Y")

  protected def transitionMatrix: DenseMatrix[Double] = DenseMatrix.zeros[Double](hiddenStates.length, hiddenStates.length)

  protected def transitionProbabilities = Map(
    "AA" -> new Multinomial(DenseVector(0.0, 1.0)),
    "AB" -> new Multinomial(DenseVector(0.0, 1.0)),
    "BA" -> new Multinomial(DenseVector(1.0, 0.0)),
    "BB" -> new Multinomial(DenseVector(1.0, 0.0))
  )

  protected def emissionMatrix: DenseMatrix[Double] = {
    val ax = 0.5
    val bx = 0.5
    DenseMatrix(
      (ax, 1-ax),
      (bx, 1-bx)
    )
  }

  override protected def emissionDists = {
    (0 until emissionMatrix.rows).map(i => {
      new Multinomial(emissionMatrix(i, ::).t)
    })
  }

  override protected def makeSequence(): (Seq[Int], Seq[Int]) = {

    val secondDist = Map(
      "A" -> new Multinomial(DenseVector(0.0, 1.0)),
      "B" -> new Multinomial(DenseVector(1.0, 0.0))
    )

    val length = sampleLength()
    var hiddenSample = Seq[Int](initDist.draw())
    hiddenSample :+= secondDist(getLastUnigram(hiddenSample)).draw()

    val hiddenOut = (1 until length - 1).foldLeft(hiddenSample)((b, _) => {
      val bigram = getLastBigram(b)
      val newState = transitionProbabilities(bigram).draw()
      b :+ newState
    })

    val visibleSample: Seq[Int] = hiddenOut.map(hiddenState => {
      emissionDists(hiddenState).draw()
    })

    (hiddenOut, visibleSample)
  }

  private def getLastUnigram(seq: Seq[Int]): String = {
    getLastNGram(1, seq)
  }

  private def getLastBigram(seq: Seq[Int]): String = {
    getLastNGram(2, seq)
  }

  private def getLastNGram(n: Int, seq: Seq[Int]): String = {
    seq.takeRight(n).map(hiddenStates.apply).mkString("")
  }
}
