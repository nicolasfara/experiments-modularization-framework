package it.unibo.sim

import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.ID
import it.unibo.scafi.space.Point3D.toPoint2D
import it.unibo.scafi.space.optimization.RichPoint3D
import it.unibo.scafi.space.{Point2D, Point3D}

import scala.jdk.CollectionConverters.CollectionHasAsScala

class SteeringService extends MyAggregateProgram {
  private lazy val needsInterventionMolecule = new SimpleMolecule("needsIntervention")
  private lazy val thisNode = node.asInstanceOf[SimpleNodeManager[Any]].node

  override def main(): Any = {
    val isRescuer = senseOr[Boolean]("isRescuer", false)
    val needsIntervention = senseOr[Boolean]("needsIntervention", false)
    val emergencyServiceResult = senseOr[ID]("it.unibo.sim.EmergencyService", -1)

//    val directionVector = G_along[Point3D](emergencyServiceResult, nbrRange _, Point3D.Zero, v => v + nbrVector())

    if (isRescuer) {
      val nearestNode = alchemistEnvironment.getNodes.stream().toList.asScala
        .filter(n => n.getConcentration(needsInterventionMolecule) == true && n.getId != mid())
        .map(n => n -> alchemistEnvironment.getDistanceBetweenNodes(thisNode, n))
        .minByOption(_._2)

      nearestNode match {
        case Some((n, _)) =>
          if (n.getId == emergencyServiceResult) node.put("rmsError", 0) else node.put("rmsError", 1)
//          val nearestNodePosition = alchemistEnvironment.getPosition(n)
//          val vector = Point2D(nearestNodePosition.getCoordinate(0), nearestNodePosition.getCoordinate(1)) - toPoint2D(currentPosition())
//          node.put("realDirection", toPoint2D(vector))
//          node.put("vectorDirection", toPoint2D(directionVector))
//          val rmsError = Math.sqrt(Math.pow(vector.x - directionVector.x, 2) + Math.pow(vector.y - directionVector.y, 2))

        case None =>
          node.put("rmsError", 0)
//          if (node.has("realDirection")) node.remove("realDirection")
//          if (node.has("vectorDirection")) node.remove("vectorDirection")
      }
    }

//    val availableRescuers = C[Double, Set[(Double, ID)]](
//      emergencyServiceResult,
//      _ ++ _,
//      mux(isRescuer) { Set(distanceToEmergency -> mid()) } { Set() },
//      Set()
//    )
//    Point2D(0, 0) + Point2D(1, 2)
//
//    val candidateRescuer = availableRescuers.minOption.getOrElse((Double.PositiveInfinity, Int.MaxValue))._2
//
//    val rescuerDecision = G[ID](needsIntervention, candidateRescuer, identity, nbrRange _)
//    val parentId = findParent(emergencyServiceResult)
//
//
//    val targetPosition = if (rescuerDecision == mid() && parentId != Int.MaxValue)
//      alchemistEnvironment.getPosition(alchemistEnvironment.getNodeByID(parentId))
//    else currentPosition()
//    node.put("movementTarget", targetPosition)
  }
}
