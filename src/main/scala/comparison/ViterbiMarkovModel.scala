package comparison

import app.Constants
import input.ConllParser
import pos.components.{SentenceBuilder, Token}

object ViterbiMarkovModel extends App {
  val model = new ViterbiMarkovModel()
  model.trainAndTestFromFiles(Constants.SMALL_TRAIN_FILE, Constants.MINI_TEST_FILE)
}

class ViterbiMarkovModel extends MarkovModel {
  override def inference(observations: Seq[Observation],
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
}
