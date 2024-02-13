package it.unibo.sim

import it.unibo.alchemist.model.{Environment, Position}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class DilateModule extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with BlockG with Gradients with FieldUtils with ExplicitFields {

  import Field._
  /**
   * Taking a field resulting from the gradient between two points, this module
   * computes the channel with a width.
   * @return a field of boolean representing if the node is parte of the channel.
   */
  override def main(): Boolean = {
    implicit val env: Environment[Any, Position[_]] = alchemistEnvironment
    /* Receiving the gradient field */
    val source: Boolean = infrastructureNeighbourReceive("source", ???).get
    val destination: Boolean = infrastructureNeighbourReceive("destination", ???).get
    val gradientField: Field[Double] = infrastructureNeighbourReceive("gradient", ???).get
    val width: Double = infrastructureNeighbourReceive("width", ???).get
    /* End of receiving the gradient field */

    val localDistanceValue = nbr { fieldToLocal(gradientField) } // Re-build the field (?)
    val result = localDistanceValue <= distanceBetween(source, destination) + width

    /* Simulate middleware message interaction */
    infrastructureNeighbourSend("channel", fnbr { result }, ???)
    /* End of middleware message interaction */
  }
}
