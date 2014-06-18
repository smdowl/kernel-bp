package app

import dependencies.components.TreeBuilder
import input.ConllParser
import dependencies.parser.HistoryParser
import dependencies.features.extractors.{DPPaperFeatureExtractor, BasicFeatureExtractor}
import dependencies.features.labellers.EdgeOnlyDataLabeller
import dependencies.parser.classifiers.KNNClassifier

object Main extends App {
  val treeBuilder = new TreeBuilder(new ConllParser())
  val historyParser = new HistoryParser()
  val featureExtractor = new DPPaperFeatureExtractor()
  val dataLabeller = new EdgeOnlyDataLabeller()

  testFeatures()

//  runTest()

  def testFeatures() = {
    val file = Constants.DEP_TEST
    val parseHistories = getHistories(file)
    val history = parseHistories(0)

    val features = history.contexts.map(context => {
      featureExtractor.extractFeatures(context)
    })

    dataLabeller.initialise(history.parseDecisions)
    val labels = history.parseDecisions.map(decision => {
      dataLabeller.label(decision)
    })

    (history.contexts zip features).foreach(pair => {
      println(s"# -> ${pair._2}")
    })
  }

  def runTest() = {
    val trainFile = Constants.TRAIN_FILE
    val testFile = Constants.MINI_TEST_FILE

    val parseHistories = getHistories(trainFile)
    val testHistories = getHistories(testFile)

    val knnClassifier = new KNNClassifier(3, featureExtractor, dataLabeller)
    knnClassifier.train(parseHistories)

    var total = 0
    var right = 0
    testHistories.foreach(testHistory => {
      (testHistory.contexts zip testHistory.parseDecisions).foreach(pair => {
        val predicted = knnClassifier.getParseDecision(pair._1)
        val actual = pair._2
        //    assert(predicted.equals(actual), "Should return same.")

        //      println(s"${predicted} => ${actual}")
        total += 1
        right += (if (predicted.equals(actual)) 1 else 0)
      })
    })

    println(s"Total $total\nRight: $right")
  }

  def getHistories(file: String) = {
    val trees = treeBuilder.buildTreesFromFile(file)
    trees.map(historyParser.parseHistory)
  }
}
