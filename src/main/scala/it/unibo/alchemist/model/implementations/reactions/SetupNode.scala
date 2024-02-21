package it.unibo.alchemist.model.implementations.reactions

import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.{Environment, Position, Time, TimeDistribution}
import org.apache.commons.math3.random.RandomGenerator

import scala.jdk.CollectionConverters._

class SetupNode[T, P <: Position[P]](
  environment: Environment[T, P],
  distribution: TimeDistribution[T],
  randomGenerator: RandomGenerator,
  seed: Int,
  cloudId: Int,
  terminationTime: Double = 3600,
) extends AbstractGlobalReaction[T, P](environment, distribution) {

  private val sourceNode = new SimpleMolecule("source")
  private val isSource = new SimpleMolecule("isSource")
  private val destinationNode = new SimpleMolecule("destination")
  private val isDestination = new SimpleMolecule("isDestination")
  private val isCloud = new SimpleMolecule("isCloud")
  private val isOffloading = new SimpleMolecule("isOffloading")
  private val offloadingMapping = new SimpleMolecule("offloadingMapping")
  private val isRescuer = new SimpleMolecule("isRescuer")
  private val isUser = new SimpleMolecule("isUser")

  private lazy val rescuerNodes: Int = Math.ceil(environment.getNodes.size() * 0.10).toInt
  private lazy val userNodes: Int = environment.getNodeCount - rescuerNodes - 1 // the cloud is not a user

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
    val mapping = environment.getNodes.stream().findFirst().get().getConcentration(offloadingMapping).asInstanceOf[Map[(String, Int), Int]]
    val nodesRequestOffloading = mapping.map { case ((_, nodeId), _) => nodeId }.toSet
    nodesRequestOffloading
      .foreach(nodeId => environment.getNodeByID(nodeId).setConcentration(isOffloading, true.asInstanceOf[T]))

    import scala.util.Random
    Random.setSeed(seed)

    // select rescuer and user nodes
    val candidate = (1 until environment.getNodeCount).toList
    val shuffledCandidate = Random.shuffle(candidate)
    val rescuerNodes = shuffledCandidate.take(Math.ceil(environment.getNodeCount * 0.10).toInt)
    val userNodes = shuffledCandidate.diff(rescuerNodes)

    userNodes.foreach(environment.getNodeByID(_).setConcentration(isUser, true.asInstanceOf[T]))
    rescuerNodes.foreach(environment.getNodeByID(_).setConcentration(isRescuer, true.asInstanceOf[T]))

    val nodesRequiringIntervention = Random.shuffle(userNodes).take(Math.ceil(userNodes.size * 0.50).toInt)
    nodesRequiringIntervention.foreach { userNode =>
      val interventionTime = randomGenerator.nextDouble() * terminationTime
      environment.getNodeByID(userNode).setConcentration(new SimpleMolecule("interventionTime"), interventionTime.asInstanceOf[T])
    }

//    val candidateNodes = environment.getNodes.stream().filter(n => n.getId != cloudId).toList
//    val rescuerSelectedNodes = Random.shuffle(candidateNodes).take(rescuerNodes) // shuffle the list to take random rescuers
//    val userSelectedNodes = candidateNodes.diff(rescuerSelectedNodes)
//    assert(rescuerSelectedNodes.size == rescuerNodes)
//    assert(userSelectedNodes.size == userNodes)

//    rescuerSelectedNodes.foreach(_.setConcentration(isRescuer, true.asInstanceOf[T]))
//    userSelectedNodes.foreach(_.setConcentration(isUser, true.asInstanceOf[T]))
//
//    // Define intervention time for each user
//    val nodesRequiringIntervention = Random.shuffle(userSelectedNodes).take(Math.ceil(userNodes * 0.50).toInt)
//    nodesRequiringIntervention.foreach { userNode =>
//      val interventionTime = randomGenerator.nextDouble() * terminationTime
//      userNode.setConcentration(new SimpleMolecule("interventionTime"), interventionTime.asInstanceOf[T])
//    }

    // Setup "movementTarget" to the local position of each node
    environment.getNodes.stream().forEach { node =>
      val position = environment.getPosition(node)
      node.setConcentration(new SimpleMolecule("movementTarget"), position.asInstanceOf[T])
    }
  }
}
