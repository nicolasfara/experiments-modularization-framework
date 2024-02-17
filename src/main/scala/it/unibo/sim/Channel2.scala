package it.unibo.sim

class Channel2 extends MyAggregateProgram {
  override def main(): Any = {
    val source = sense[Int]("source") == mid()
    val destination = sense[Int]("destination") == mid()
    val width = sense[Double]("channelWidth")

    val localDistanceValue = node.getOrElse[Double]("it.unibo.sim.Channel1", Double.PositiveInfinity)
    val distanceBetweenSourceAndDestination = distanceBetween(source, destination)
    node.put("distanceBetween", distanceBetweenSourceAndDestination)

    val result = localDistanceValue <= distanceBetweenSourceAndDestination + width
    node.put("inChannel", result)
    result
  }
}