package it.unibo.sim

import scala.jdk.OptionConverters.RichOptional

class SteeringService extends MyAggregateProgram {
  override def main(): Any = {
    val isRescuer = senseOr[Boolean]("isRescuer", false)
    val emergencyServiceResult = senseOr[Double]("it.unibo.sim.EmergencyService", Double.PositiveInfinity)
    val parentNodeId = findParent(emergencyServiceResult)

    // Update rescue target position
    alchemistEnvironment.getNodes.stream()
      .filter(n => n.getId == parentNodeId).findFirst().toScala.foreach(parentNode => {
        val parentPosition = alchemistEnvironment.getPosition(parentNode)
        if (isRescuer) node.put("movementTarget", parentPosition)
      })
  }
}
