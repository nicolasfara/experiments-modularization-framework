package it.unibo.sim

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID

class EmergencyService extends MyAggregateProgram {
  override def main(): Any = {
    val currentSimTime = alchemistEnvironment.getSimulation.getTime.toDouble
    val interventionTime = senseOr[Double]("interventionTime", Double.PositiveInfinity)
    val isRescuer = senseOr[Boolean]("isRescuer", false)

    rep(false) { intervened =>
      val isInterventionRequired = currentSimTime >= interventionTime
      node.put("needsIntervention", isInterventionRequired)
      val isRescuerIntervened = foldhood(false)(_ || _)(nbr { isRescuer })

      val moduleResult = G[(ID, Boolean)](isInterventionRequired, nbr { mid -> isInterventionRequired }, identity, nbrRange _)
      node.put("it.unibo.sim.EmergencyService", moduleResult)

      isInterventionRequired && !(intervened || isRescuerIntervened)
    }
  }
}
