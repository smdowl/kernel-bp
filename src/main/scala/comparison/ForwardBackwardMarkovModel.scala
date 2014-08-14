package comparison

import app.Constants
import breeze.linalg.sum

object ForwardBackwardMarkovModel extends App {
  val model = new ForwardBackwardMarkovModel()
  model.trainAndTestFromFiles(Constants.MINI_TEST_FILE, Constants.MINI_TEST_FILE)
}

class ForwardBackwardMarkovModel extends MarkovModel {

  type Probabilities = Map[String, Probability]

  private var previousProbs: Probabilities = _

  override def inference(observations: Seq[Observation]) = {
    val length = observations.length

    var forward = Seq[Probabilities]()
    previousProbs = Map[State, Probability]()

    def calcSumProb(state: State) = {
      sum(states.map(prev => previousProbs(prev) * transitions((prev, state))))
    }

    for ((observation, idx) <- observations.zipWithIndex) {
      var probabilities = Map[State, Probability]()

      for (state <- states) {
        val prevProbSum = if (idx == 0)
          initDist(state)
        else
          calcSumProb(state)

        val newProb = emissions((state, observation)) * prevProbSum
        probabilities += state -> newProb
      }

      forward :+= probabilities
      previousProbs = probabilities
    }

    println(previousProbs)

    null
  }

  private def getProbabilitiesForIndex(observation: Observation, idx: Int) = {

  }
}
