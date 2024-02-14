package it.unibo.sim

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import scala.concurrent.duration.FiniteDuration

class Channel1 extends MyAggregateProgram {
  override def main(): Any = {
    val sourceId = sense[Int]("source")
    val destinationId = sense[Int]("destination")
    val width = sense[Double]("width")
    val result = distanceTo(sourceId == mid()) + distanceTo(mid() == destinationId)
    result
  }
}