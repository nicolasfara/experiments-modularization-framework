package it.unibo.scafi.sim

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

import scala.concurrent.duration.FiniteDuration

class Channel2 extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with BlockG with Gradients with FieldUtils {
  override def main(): Any = {
    val source = sense[Int]("source") == mid()
    val destination = sense[Int]("destination") == mid()
    val width = sense[Double]("width")
    val localDistanceValue = sense[Double]("it.unibo.scafi.sim.Channel1")
    val result = localDistanceValue <= distanceBetween(source, destination) + width
    result
  }
}