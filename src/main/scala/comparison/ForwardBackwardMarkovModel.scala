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

    def calcForwardSumProb(state: State) = {
      sum(states.map(prev => previousProbs(prev) * transitions((prev, state))))
    }

    for ((observation, idx) <- observations.zipWithIndex) {
      var probabilities = Map[State, Probability]()

      for (state <- states) {
        val prevProbSum = if (idx == 0)
          initDist(state)
        else
          calcForwardSumProb(state)

        val newProb = emissions((state, observation)) * prevProbSum
        probabilities += state -> newProb
      }

      forward :+= probabilities
      previousProbs = probabilities
    }

    println(previousProbs)
    val endState = previousProbs.keys.maxBy(previousProbs(_))
    val probForward = calcForwardSumProb(endState)

    var backward = Seq[Probabilities]()
    previousProbs = Map[State, Probability]()

    def calcBackwardSumProb(state: State, obs: Observation) = {
      sum(states.map(next => previousProbs(next) * transitions((state, next)) * emissions((next, obs))))
    }

    val reversedSeq = (observations.drop(1) :+ "_").reverse
    for ((observation, idx) <- reversedSeq.zipWithIndex) {
      var probabilities = Map[State, Probability]()

      for (state <- states) {
        val prevProbSum = if (idx == 0)
          transitions(state, endState)
        else
          calcBackwardSumProb(state, observation)

        val newProb = emissions((state, observation)) * prevProbSum
        probabilities += state -> newProb
      }

      backward :+= probabilities
      previousProbs = probabilities
    }

    val probBackward = sum(states.map(calcBackwardSumProb(_, observations(0))))

    var posterior = Seq[Probabilities]()
    for (i <- 0 until length) {
      val probs = states.map(state => {
        state -> forward(i)(state) * backward(i)(state) / probForward
      }).toMap

      posterior :+= probs
    }

//    assert(probForward == probBackward)

    val bestSequence = posterior.map(probs => probs.keys.maxBy(probs(_)))

    null
  }
}
