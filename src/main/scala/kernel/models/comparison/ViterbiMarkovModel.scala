package kernel.models.comparison

import app.Constants
import input.ConllParser
import pos.components.{Token, SentenceBuilder}

object ViterbiMarkovModel extends App {
  val model = new ViterbiMarkovModel()
  model.train(Constants.SMALL_TRAIN_FILE, Constants.MINI_TEST_FILE)
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
    val observations = stripObservations(testSentences)
    val correctTags = stripTags(testSentences)

    val results = (observations zip correctTags).flatMap{ case (observation, correct) =>
      val path = viterbi(observation, states, initProbs, transition, emission)

      val results = (correct zip path._2).map { case (a, b) =>
        a.equals(b)
      }
      results
    }

    val accuracy = results.foldLeft(0.0)((sum, correct) => sum + (if (correct) 1.0 else 0.0)) / results.length
    println(accuracy)
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
              initDist: State => Probability,
              transitions: ProbabilityMap,
              emissions: EmissionMap): ProbabilityPath = {

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

  private def stripObservations(sentences: Seq[Seq[Token]]) = {
    sentences.map(sentence => {
      sentence.map(_.form)
    })
  }

  private def stripTags(sentences: Seq[Seq[Token]]) = {
    sentences.map(sentence => {
      sentence.map(_.POS)
    })
  }
}
