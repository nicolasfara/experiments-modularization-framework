package it.unibo.sim

import it.unibo.alchemist.model.{Environment, Position}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class GradientModule extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with BlockG with Gradients with FieldUtils with ExplicitFields {

  /**
   * Module computing the gradient between a source and a destination.
   * @return a field containing the gradient values between two points.
   */
  override def main(): Field[Boolean] = {
    implicit val env: Environment[Any, Position[_]] = alchemistEnvironment
    val sourceId = sense[Int]("source")
    val destinationId = sense[Int]("destination")
    val width = sense[Double]("width")
    val result = distanceTo(sourceId == mid()) + distanceTo(mid() == destinationId)

    /* Simulate middleware message interaction */
    val outcomeField = fnbr(result)
    val _ = infrastructureNeighbourSend("source", sourceId == mid(), mid())
    val _ = infrastructureNeighbourSend("destination", destinationId == mid(), mid())
    val _ = infrastructureNeighbourSend("width", width, mid())
    val _ = infrastructureNeighbourSend("gradient", outcomeField, mid())
    /* End of middleware message interaction */

    /* Receiving the channel field */
    val channelField: Field[Boolean] = infrastructureNeighbourReceive("channel", mid()).getOrElse(Field(Map()))

    channelField
    /* End of receiving the channel field */
  }
}
