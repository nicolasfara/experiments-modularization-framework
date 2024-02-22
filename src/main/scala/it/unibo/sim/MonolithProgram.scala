package it.unibo.sim

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID

class MonolithProgram extends MyAggregateProgram {
  override def main(): Unit = {
    val currentSimTime = alchemistEnvironment.getSimulation.getTime.toDouble
    val interventionTime = senseOr[Double]("interventionTime", Double.PositiveInfinity)
    val isRescuer = senseOr[Boolean]("isRescuer", false)

    val initial = (Double.PositiveInfinity, Double.PositiveInfinity)

    rep(initial) { case (lastInterventionTime, _) =>
      val isInterventionRequired = currentSimTime >= interventionTime && !(currentSimTime >= lastInterventionTime)
      node.put("needsIntervention", isInterventionRequired)
      val isRescuerIntervened = foldhood(isRescuer)(_ || _)(nbr { isRescuer })

      val moduleResult = classicGradient(isInterventionRequired)

      if (isInterventionRequired && isRescuerIntervened) {
        node.put("requiredInterventionTime", currentSimTime - interventionTime)
        node.put("saved", true)
      }

      val distanceToEmergency = distanceTo(isInterventionRequired, nbrRange _)
      node.put("distanceToEmergency", distanceToEmergency)

      val availableRescuers = C[Double, Set[(Double, ID)]](
        moduleResult,
        _ ++ _,
        mux(isRescuer) { Set(distanceToEmergency -> mid()) } { Set() },
        Set()
      )
      node.put("availableRescuers", availableRescuers)

      val candidateRescuer = availableRescuers.minOption.getOrElse((Double.PositiveInfinity, Int.MaxValue))._2

      val rescuerDecision = G[ID](isInterventionRequired, candidateRescuer, identity, nbrRange _)
      node.put("rescuerDecision", rescuerDecision)
      val parentId = findParent(moduleResult)

      val targetPosition = if (rescuerDecision == mid() && parentId != Int.MaxValue)
        alchemistEnvironment.getPosition(alchemistEnvironment.getNodeByID(parentId))
      else currentPosition()
      node.put("movementTarget", targetPosition)

      val time = if (isRescuerIntervened) currentSimTime else lastInterventionTime
      (time, moduleResult)
    }
  }
}
