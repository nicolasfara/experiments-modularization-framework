package it.unibo.sim

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import scala.concurrent.duration.FiniteDuration

class Channel2 extends MyAggregateProgram {
  override def main(): Any = {
    val source = sense[Int]("source") == mid()
    val destination = sense[Int]("destination") == mid()
    val width = sense[Double]("width")
    val localDistanceValue = senseOr[Double]("it.unibo.sim.Channel1", Double.PositiveInfinity)
    val distanceBetweenSourceAndDestination = distanceBetween(source, destination)
    node.put("distanceBetween", distanceBetweenSourceAndDestination)
    val result = localDistanceValue <= distanceBetweenSourceAndDestination + width
    result
  }
}