package pos.components

import breeze.linalg.DenseVector

case class Token(id: Int,
                 form: String,
                 POS: String,
                 wordVector: DenseVector[Double] = null)
