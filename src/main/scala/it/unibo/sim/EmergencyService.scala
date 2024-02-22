package it.unibo.sim

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID

class EmergencyService extends MyAggregateProgram {
  override def main(): Any = {
    val currentSimTime = alchemistEnvironment.getSimulation.getTime.toDouble
    val interventionTime = senseOr[Double]("interventionTime", Double.PositiveInfinity)
    val isRescuer = senseOr[Boolean]("isRescuer", false)

    val initial = (Double.PositiveInfinity, Double.PositiveInfinity)

    val isInterventionRequired = currentSimTime >= interventionTime
    node.put("needsIntervention", isInterventionRequired)
    val isRescuerIntervened = foldhood(isRescuer)(_ || _)(nbr { isRescuer })

    val moduleResult = broadcast(isInterventionRequired, mid(), nbrRange _) // classicGradient(isInterventionRequired)

    if (isInterventionRequired && isRescuerIntervened) {
      node.put("requiredInterventionTime", currentSimTime - interventionTime)
      node.put("saved", true)
    }
    moduleResult
  }
}
