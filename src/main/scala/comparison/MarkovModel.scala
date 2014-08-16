package comparison

import app.Constants
import breeze.linalg.sum
import input.ConllParser
import pos.components.{SentenceBuilder, Token}

object MarkovModel extends App {
  val model = new MarkovModel()
  model.trainAndTestFromFiles(Constants.MINI_TEST_FILE, Constants.MINI_TEST_FILE)
}

class MarkovModel {
  type State = String
  type Observation = String
  type Probability = Double
  type InitProbabilityMap = State => Probability
  type ProbabilityMap = ((State, State)) => Probability
  type EmissionMap = ((State, Observation)) => Probability
  type ProbabilityPath = (Probability, List[State])

  var states: Seq[State] = _

  var initDist: InitProbabilityMap = _
  var transitions: ProbabilityMap = _
  var emissions: EmissionMap = _

  def trainAndTestFromFiles(filepath: String, testFilepath: String) = {
    val parser = new ConllParser()
    val builder = new SentenceBuilder(parser)

    val sentences = builder.buildSentenceFromFile(filepath)

    val splitSentences = this.splitSentences(sentences)

    train(splitSentences)

    val testSentences = this.splitSentences(builder.buildSentenceFromFile(testFilepath))

    val viterbiResults = testSentences.flatMap(testSentence(viterbi, _))
    val fbResults = testSentences.flatMap(testSentence(forwardBackward, _))

    val vitAccuracy = calcAccuracy(viterbiResults)
    val fbAccuracy = calcAccuracy(fbResults)
    println(s"Viterbi Accuracy: $vitAccuracy")
    println(s"FB Accuracy: $fbAccuracy")
  }

  private def calcAccuracy(results: Seq[Boolean]) = {
    results.foldLeft(0.0)((sum, actual) => sum + (if (actual) 1.0 else 0.0)) / results.length
  }

  private def splitSentences(sentences: Seq[Seq[Token]]) = sentences.map(sentence => {
    sentence.map(token => {
      (token.POS, token.form)
    })
  })

  def viterbiTestSentence(pairs: Seq[(State, Observation)]) = {
    testSentence(viterbi, pairs)
  }

  def forwardBackwardTestSentence(pairs: Seq[(State, Observation)]) = {
    testSentence(forwardBackward, pairs)
  }

  def testSentence(function: Seq[Observation] => ProbabilityPath, pairs: Seq[(State, Observation)]) = {
    val correct = pairs.map(_._1)
    val visible = pairs.map(_._2)

    val path = function(visible)
    val results = (correct zip path._2).map { case (a, b) =>
      a.equals(b)
    }
    results
  }

  def train(sentences: Seq[Seq[(State, Observation)]]) = {
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

      val initPos = sentence(0)._1
      initCounts = increment(initCounts, initPos)

      for ((pos, form) <- sentence) {
        if (!states.contains(pos))
          states :+= pos
        if (!emissions.contains(form))
          emissions :+= form

        val emission = (pos, form)
        emissionCounts = increment(emissionCounts, emission)

        if (prev != null) {
          val trans = (prev, pos)
          transCounts = increment(transCounts, trans)
        }
        prev = pos
      }
    }
    initCounts = normaliseInit(initCounts, numSentences)

    emissionCounts = normaliseTuples(emissionCounts, numSamples)
    transCounts = normaliseTuples(transCounts, numSamples)

    initDist = getProbDist(initCounts)
    transitions = getProbDist(transCounts)
    this.emissions = getProbDist(emissionCounts)
  }

  private def increment[T](map: Map[T, Probability], key: T): Map[T, Probability] = {
    map + (key -> (map.getOrElse(key, 0.0) + 1.0))
  }

  private def normaliseInit(map: Map[State, Probability], normaliser: Probability): Map[State, Probability] = {
    var out = map
    for (key <- map.keys)
      out += key -> (map(key) / normaliser)
    out
  }

  private def normaliseTuples(map: Map[(State, Observation), Probability],
                              normaliser: Probability): Map[(State, Observation), Probability] = {
    var normalisers = Map[State, Probability]()

    for (key <- map.keys) {
      normalisers += key._1 -> (normalisers.getOrElse(key._1, 0.0) + map(key))
    }

    var out = map
    for (key <- map.keys)
      out += key -> (map(key) / normalisers(key._1))
    out
  }

  private def getProbDist[T](map: Map[T, Probability]): T => Probability = {
    key: T => {
      map.getOrElse(key, 1e-6)
    }
  }

  def viterbi(observations: Seq[Observation]): ProbabilityPath = {

    def probability(p: ProbabilityPath) = p._1

    val initial = states map { (state) =>
      (initDist(state) * emissions((state, observations(0))), List(state))
    } maxBy probability

    val probPath = observations.tail.foldLeft(initial)((probPath: ProbabilityPath, observation: Observation) => {
      states map { (state) =>
        val prevState = probPath._2.head
        val transition = (prevState, state)
        val emission = (state, observation)

        (probPath._1 * transitions(transition) * emissions(emission), state :: probPath._2)
      } maxBy probability
    })

    (probPath._1, probPath._2.reverse)
  }

  type Probabilities = Map[String, Probability]

  private var previousProbs: Probabilities = _

  def forwardBackward(observations: Seq[Observation]): ProbabilityPath = {
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
    (probBackward, bestSequence.toList)
  }
}
