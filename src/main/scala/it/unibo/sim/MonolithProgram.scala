package it.unibo.sim

import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.scafi.space.Point3D.toPoint2D
import it.unibo.scafi.space.{Point2D, Point3D}
import it.unibo.scafi.space.optimization.RichPoint3D

import scala.jdk.CollectionConverters.CollectionHasAsScala

class MonolithProgram extends MyAggregateProgram {
  private lazy val needsInterventionMolecule = new SimpleMolecule("needsIntervention")
  private lazy val thisNode = node.asInstanceOf[SimpleNodeManager[Any]].node

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

      val directionVector = G_along[Point3D](moduleResult, nbrRange _, Point3D.Zero, v => v + nbrVector())

      if (isRescuer) {
        val nearestNode = alchemistEnvironment.getNodes.stream().toList.asScala
          .filter(n => n.getConcentration(needsInterventionMolecule) == true && n.getId != mid())
          .map(n => n -> alchemistEnvironment.getDistanceBetweenNodes(thisNode, n))
          .minByOption(_._2)

        nearestNode match {
          case Some((n, _)) =>
            val nearestNodePosition = alchemistEnvironment.getPosition(n)
            val vector = Point2D(nearestNodePosition.getCoordinate(0), nearestNodePosition.getCoordinate(1)) - toPoint2D(currentPosition())
            node.put("realDirection", toPoint2D(vector))
            node.put("vectorDirection", toPoint2D(directionVector))
            val rmsError = Math.sqrt(Math.pow(vector.x - directionVector.x, 2) + Math.pow(vector.y - directionVector.y, 2))
            node.put("rmsError", rmsError)
          case None =>
            if (node.has("realDirection")) node.remove("realDirection")
            if (node.has("vectorDirection")) node.remove("vectorDirection")
        }
      }

      val time = if (isRescuerIntervened) currentSimTime else lastInterventionTime
      (time, moduleResult)
    }
  }
}
