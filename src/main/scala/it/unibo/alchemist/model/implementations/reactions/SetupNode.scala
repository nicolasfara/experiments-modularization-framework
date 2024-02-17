package it.unibo.alchemist.model.implementations.reactions

import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.{Environment, Position, Time, TimeDistribution}

class SetupNode[T, P <: Position[P]](
  environment: Environment[T, P],
  distribution: TimeDistribution[T],
  cloudId: Int,
) extends AbstractGlobalReaction[T, P](environment, distribution) {

  private val sourceNode = new SimpleMolecule("source")
  private val isSource = new SimpleMolecule("isSource")
  private val destinationNode = new SimpleMolecule("destination")
  private val isDestination = new SimpleMolecule("isDestination")
  private val isCloud = new SimpleMolecule("isCloud")

  override def initializationComplete(time: Time, environment: Environment[T, _]): Unit =
    getTimeDistribution.update(Time.INFINITY, true, 0.0, environment)

  override protected def executeBeforeUpdateDistribution(): Unit = {
    environment.getNodes.stream()
      .filter(node => node.getConcentration(sourceNode) == node.getId)
      .forEach(_.setConcentration(isSource, true.asInstanceOf[T]))
    environment.getNodes.stream()
      .filter(node => node.getConcentration(destinationNode) == node.getId)
      .forEach(_.setConcentration(isDestination, true.asInstanceOf[T]))
    environment.getNodeByID(cloudId).setConcentration(isCloud, true.asInstanceOf[T])
  }
}
