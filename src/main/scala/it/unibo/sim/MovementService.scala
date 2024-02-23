package it.unibo.sim

import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID

class MovementService extends MyAggregateProgram {
  private lazy val needsInterventionMolecule = new SimpleMolecule("needsIntervention")
  private lazy val thisNode = node.asInstanceOf[SimpleNodeManager[Any]].node

  override def main(): Unit = {
    val isRescuer = senseOr[Boolean]("isRescuer", false)
    val needsIntervention = senseOr[Boolean]("needsIntervention", false)
    val emergencyServiceResult = senseOr[Double]("it.unibo.sim.EmergencyGradientService", Double.PositiveInfinity)

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
