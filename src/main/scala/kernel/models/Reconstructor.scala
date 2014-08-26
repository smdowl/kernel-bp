package kernel.models

object Reconstructor {
  def reconstructTrainingData(kernelModel: Model) = {
    val keys = kernelModel.keyArray
    val data = kernelModel.trainData

    var output = Seq[Seq[(String, String)]]()

    for (sentence <- data) {
      val hidden = sentence.slice(0, sentence.length / 2)
      val visible = sentence.slice(sentence.length / 2, sentence.length)

      var hiddenSeq = Seq[String]()
      var visibleSeq = Seq[String]()

      for (node <- hidden) {
        val featureIdxs = node.findAll(_.equals(1.0))
        for (featureIdx <- featureIdxs) {
          val key = keys(featureIdx).split(":")(0)
          if (key.equals("label"))
            hiddenSeq :+= keys(featureIdx).split(":")(1)
        }
      }

      for (node <- visible) {
        val featureIdxs = node.findAll(_.equals(1.0))

        var added = false
        for (featureIdx <- featureIdxs) {
          val key = keys(featureIdx).split(":")(0)
          if (key.equals("form")) {
            visibleSeq :+= keys(featureIdx).split(":")(1)
            added = true
          }
        }
        if (!added)
          visibleSeq :+= "NONE"
      }

      output :+= (hiddenSeq zip visibleSeq)
    }

    output
  }

  def reconstructTestData(kernelModel: Model) = {
    val keys = kernelModel.keyArray
    val observations = kernelModel.testObservations
    val labels = kernelModel.testLabels

    var output = Seq[Seq[(String, String)]]()

    for ((hidden, visible) <- labels zip observations) {
      var hiddenSeq = Seq[String]()
      var visibleSeq = Seq[String]()

      for (nodeIdx <- 0 until hidden.size) {
        val node = hidden(nodeIdx).toDenseMatrix
        val featureIdxs = node.findAll(_.equals(1.0)).map(_._2)
        for (featureIdx <- featureIdxs) {
          val key = keys(featureIdx).split(":")(0)
          if (key.equals("label"))
            hiddenSeq :+= keys(featureIdx).split(":")(1)
        }
      }
      for (nodeIdx <- visible.size until 2 * visible.size) {
        val node = visible(nodeIdx).toDenseMatrix
        val featureIdxs = node.findAll(_.equals(1.0)).map(_._2)
        var added = false
        for (featureIdx <- featureIdxs) {
          val key = keys(featureIdx).split(":")(0)
          if (key.equals("form")) {
            visibleSeq :+= keys(featureIdx).split(":")(1)
            added = true
          }
        }
        if (!added)
          visibleSeq :+= "NONE"
      }

      output :+= (hiddenSeq zip visibleSeq)
    }

    output
  }
}
