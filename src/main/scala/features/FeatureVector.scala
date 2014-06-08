package features

import scala.collection.mutable

class FeatureVector(values: Map[String, Double] = Map()) {

  def dot(other: FeatureVector) = {
    val output = mutable.Map[String, Double]()
    keySet.intersect(other.keySet).foreach(key => {
      output += (key -> this(key) * other(key))
    })

    new FeatureVector(output.toMap)
  }

  def keys = values.keys
  def keySet = values.keySet
  def apply(key: String) = values(key)
}
