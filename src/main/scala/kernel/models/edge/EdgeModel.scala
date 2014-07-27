package kernel.models.edge

abstract class EdgeModel {
  def generateEdges(): Map[String, Edge]
}
