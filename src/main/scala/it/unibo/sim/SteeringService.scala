package it.unibo.sim

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID

class SteeringService extends MyAggregateProgram {
  override def main(): Any = {
    val isRescuer = senseOr[Boolean]("isRescuer", false)
    val needsIntervention = senseOr[Boolean]("needsIntervention", false)
    val emergencyServiceResult = senseOr[Double]("it.unibo.sim.EmergencyService", Double.PositiveInfinity)

    val distanceToEmergency = distanceTo(needsIntervention, nbrRange _)

    val availableRescuers = C[Double, Set[(Double, ID)]](
      emergencyServiceResult,
      _ ++ _,
      mux(isRescuer) { Set(distanceToEmergency -> mid()) } { Set() },
      Set()
    )

    val candidateRescuer = availableRescuers.minOption.getOrElse((Double.PositiveInfinity, Int.MaxValue))._2

    val rescuerDecision = G[ID](needsIntervention, candidateRescuer, identity, nbrRange _)
    val parentId = findParent(emergencyServiceResult)

    val targetPosition = if (rescuerDecision == mid() && parentId != Int.MaxValue)
      alchemistEnvironment.getPosition(alchemistEnvironment.getNodeByID(parentId))
    else currentPosition()
    node.put("movementTarget", targetPosition)
  }
}
