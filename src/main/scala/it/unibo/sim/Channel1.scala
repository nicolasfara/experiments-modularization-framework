package it.unibo.scafi.examples

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import scala.concurrent.duration.FiniteDuration

class HelloScafi extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with BlockG with Gradients with FieldUtils {
  override def main(): Any = {
    val sourceId = sense[Int]("source")
    val destinationId = sense[Int]("destination")
    val width = sense[Double]("width")
    val result = distanceTo(sourceId == mid()) + distanceTo(mid() == destinationId)
    result
  }
}