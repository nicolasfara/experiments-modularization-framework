package it.unibo.sim

class EmergencyGradientService extends MyAggregateProgram {
  override def main(): Any = {
    val currentSimTime = alchemistEnvironment.getSimulation.getTime.toDouble
    val interventionTime = senseOr[Double]("interventionTime", Double.PositiveInfinity)
    val isRescuer = senseOr[Boolean]("isRescuer", false)

    val initial = (Double.PositiveInfinity, Double.PositiveInfinity)

    val (_, result) = rep(initial) { case (lastInterventionTime, _) =>
      val isInterventionRequired = currentSimTime >= interventionTime && !(currentSimTime >= lastInterventionTime)
      node.put("needsIntervention", isInterventionRequired)
      val isRescuerIntervened = foldhood(isRescuer)(_ || _)(nbr {
        isRescuer
      })

      val moduleResult = classicGradient(isInterventionRequired)

      if (isInterventionRequired && isRescuerIntervened) {
        node.put("requiredInterventionTime", currentSimTime - interventionTime)
        node.put("saved", true)
      }

      val time = if (isRescuerIntervened) currentSimTime else lastInterventionTime
      (time, moduleResult)
    }
    result
  }
}
