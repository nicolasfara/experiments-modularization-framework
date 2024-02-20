package it.unibo.sim

import it.unibo.alchemist.model.molecules.SimpleMolecule

import scala.jdk.OptionConverters.RichOptional

class SteeringService extends MyAggregateProgram {
  override def main(): Any = {
    val isRescuer = senseOr[Boolean]("isRescuer", false)
    val emergencyServiceResult = senseOr[Double]("it.unibo.sim.EmergencyService", Double.PositiveInfinity)
    val parentNodeId = findParent(emergencyServiceResult)

    // Update rescue target position
    alchemistEnvironment.getNodes.stream()
      .filter(n => n.getId == parentNodeId)
      .filter(n => n.getConcentration(new SimpleMolecule("isRescuer")) != true)
      .findFirst().toScala.foreach(parentNode => {
        val parentPosition = alchemistEnvironment.getPosition(parentNode)
        if (isRescuer) {
          node.put("movementTarget", parentPosition.plus(Array(0.01, 0.01)))
          node.put("DEBUG: effectiveParentPosition", parentPosition)
        }
      })
  }
}
