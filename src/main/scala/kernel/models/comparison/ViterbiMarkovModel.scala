package kernel.models.comparison

import app.Constants
import input.ConllParser
import pos.components.{Token, SentenceBuilder}

object ViterbiMarkovModel extends App {
  val model = new ViterbiMarkovModel()
  model.train(Constants.MINI_TRAIN_FILE, Constants.MINI_TEST_FILE)
}

class ViterbiMarkovModel {
  type State = String
  type Observation = String
  type Probability = Double
  type InitProbabilityMap = State => Probability
  type ProbabilityMap = ((State, State)) => Probability
  type EmissionMap = ((State, Observation)) => Probability
  type ProbabilityPath = (Probability, List[State])

  var states: Seq[State] = _

  def train(filepath: String, testFilepath: String) = {
    val parser = new ConllParser()
    val builder = new SentenceBuilder(parser)
    
    val sentences = builder.buildSentenceFromFile(filepath)
    val (initProbs, transition, emission) = learnTransitionAndEmissionProbabilties(sentences)

    val testSentences = builder.buildSentenceFromFile(testFilepath)
    val observations = stripObservations(sentences)

    val observation = observations(0)
    val path = viterbi(observation, states, initProbs, transition, emission)
    println(path)
  }

  def learnTransitionAndEmissionProbabilties(sentences: Seq[Seq[Token]]): (InitProbabilityMap, ProbabilityMap, EmissionMap) = {
    states = Seq[String]()
    var emissions = Seq[String]()

    var initCounts = Map[State, Probability]()
    var transCounts = Map[(State, State), Probability]()
    var emissionCounts = Map[(State, Observation), Probability]()

    var prev: String = null

    val numSentences = sentences.length
    var numSamples = 0

    for (sentence <- sentences) {
      numSamples += sentence.length

      val initPos = sentence(0).POS
      initCounts = increment(initCounts, initPos)

      for (token <- sentence) {
        if (!states.contains(token.POS))
          states :+= token.POS
        if (!emissions.contains(token.form))
          emissions :+= token.form

        val emission = (token.POS, token.form)
        emissionCounts = increment(emissionCounts, emission)

        if (prev != null) {
          val trans = (prev, token.POS)
          transCounts = increment(transCounts, trans)
        }
        prev = token.POS
      }
    }
    initCounts = normalise(initCounts, numSentences)
    emissionCounts = normalise(emissionCounts, numSamples)
    transCounts = normalise(transCounts, numSamples)

    (getProbDist(initCounts), getProbDist(transCounts), getProbDist(emissionCounts))
  }

  private def increment[T](map: Map[T, Probability], key: T): Map[T, Probability] = {
    map + (key -> (map.getOrElse(key, 0.0) + 1.0))
  }

  private def normalise[T](map: Map[T, Probability], normaliser: Probability): Map[T, Probability] = {
    var out = map
    for (key <- map.keys)
      out += key -> (map(key) / normaliser)
    out
  }

  private def getProbDist[T](map: Map[T, Probability]): T => Probability = {
    key: T => {
      map.getOrElse(key, 1e-6)
    }
  }

  def viterbi(observations: Seq[Observation],
              states: Seq[State],
              start: State => Probability,
              transition: ProbabilityMap,
              emissions: EmissionMap): ProbabilityPath = {

    def probability(p: ProbabilityPath) = p._1

    def mostLikelyPathFrom(state: State, time: Int): ProbabilityPath = {
      println(time)
      val em = (state, observations(time))
      val emission = emissions(em)

      time match {
        case 0 =>
          // (probability that were in the initial state) times
          // (probability of observing the initial observation from the initial state)
          (start(state) * emission, List(state))
        case _ =>
          val (prob, path) = states map { (state) =>
            val (prob, path) = mostLikelyPathFrom(state, time - 1)
            val prevState = path.head
            // (probability of the previous state) times
            // (probability of moving from previous state to this state)
            val trans = (prevState, state)
            (prob * transition(trans), path)
          } maxBy probability
          // (probability of observing the current observation from this state) times
          // (probability of the maximizing state)
          (emission * prob, state :: path)
      }
    }

    val (prob, path) = states map { (state) =>
      mostLikelyPathFrom(state, observations.size - 1)
    } maxBy probability

    (prob, path.reverse)
  }

  private def stripObservations(sentences: Seq[Seq[Token]]) = {
    sentences.map(sentence => {
      sentence.map(_.form)
    })
  }
}
