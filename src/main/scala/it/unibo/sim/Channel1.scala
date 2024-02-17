package it.unibo.sim

class Channel1 extends MyAggregateProgram {
  override def main(): Any = {
    val sourceId = sense[Int]("source")
    val destinationId = sense[Int]("destination")

    val isSource = sourceId == mid()
    val isDestination = destinationId == mid()
    val distanceToSource = distanceTo(isSource)
    val distanceToDestination = distanceTo(isDestination)

    node.put("distanceToSource", distanceToSource)
    node.put("distanceToDestination", distanceToDestination)

    distanceToSource + distanceToDestination
  }
}