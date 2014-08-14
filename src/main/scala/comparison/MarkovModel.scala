package comparison

import app.Constants
import input.ConllParser
import pos.components.{SentenceBuilder, Token}

abstract class MarkovModel {
  type State = String
  type Observation = String
  type Probability = Double
  type InitProbabilityMap = State => Probability
  type ProbabilityMap = ((State, State)) => Probability
  type EmissionMap = ((State, Observation)) => Probability
  type ProbabilityPath = (Probability, List[State])

  var states: Seq[State] = _

  var initProbs: InitProbabilityMap = _
  var transition: ProbabilityMap = _
  var emission: EmissionMap = _

  def trainAndTestFromFiles(filepath: String, testFilepath: String) = {
    val parser = new ConllParser()
    val builder = new SentenceBuilder(parser)

    val sentences = builder.buildSentenceFromFile(filepath)

    val splitSentences = this.splitSentences(sentences)

    train(splitSentences)

    val testSentences = this.splitSentences(builder.buildSentenceFromFile(testFilepath))

    val results = testSentences.flatMap(testSentence)

    val accuracy = results.foldLeft(0.0)((sum, actual) => sum + (if (actual) 1.0 else 0.0)) / results.length
    println(s"Accuracy: $accuracy")
  }

  private def splitSentences(sentences: Seq[Seq[Token]]) = sentences.map(sentence => {
    sentence.map(token => {
      (token.form, token.POS)
    })
  })

  def testSentence(pairs: Seq[(State, Observation)]) = {
    val correct = pairs.map(_._1)
    val visible = pairs.map(_._2)

    val path = inference(visible, states, initProbs, transition, emission)
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

    initProbs = getProbDist(initCounts)
    transition = getProbDist(transCounts)
    emission = getProbDist(emissionCounts)
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

  def inference(observations: Seq[Observation],
              states: Seq[State],
              initDist: State => Probability,
              transitions: ProbabilityMap,
              emissions: EmissionMap): ProbabilityPath
}
