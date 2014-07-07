package input

import breeze.linalg.DenseVector

case class ParsedToken(id: Int,
                      form: String,
                      lemma: String,
                      coarsePOS: String,
                      POS: String,
                      features: Seq[String],
                      head: Int,
                      depRel: String,
                      wordVector: DenseVector[Double] = null)
