package kernel.parsing

import kernel.caches.Cache
import kernel.kernels.Kernel
import kernel.models.components.Edge

abstract class EdgeParser(val kernel: Kernel) {
  def buildCache(edges: Map[String, Edge], length: Int, smooth: Boolean = false): Cache
}
