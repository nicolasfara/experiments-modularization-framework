package it.unibo.alchemist.model.implementations.reactions

import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.{Environment, Position, Time, TimeDistribution}
import org.apache.commons.math3.random.{RandomAdaptor, RandomGenerator}

import scala.jdk.CollectionConverters._

class SetupNode[T, P <: Position[P]](
  environment: Environment[T, P],
  distribution: TimeDistribution[T],
  randomGenerator: RandomGenerator,
  cloudId: Int,
  terminationTime: Double,
) extends AbstractGlobalReaction[T, P](environment, distribution) {

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
    environment.getNodeByID(cloudId).setConcentration(isCloud, true.asInstanceOf[T])
    val mapping = environment.getNodes.stream().findFirst().get().getConcentration(offloadingMapping).asInstanceOf[Map[(String, Int), Int]]
    val nodesRequestOffloading = mapping.map { case ((_, nodeId), _) => nodeId }.toSet
    nodesRequestOffloading
      .foreach(nodeId => environment.getNodeByID(nodeId).setConcentration(isOffloading, true.asInstanceOf[T]))

    // select rescuer and user nodes
    val candidate = (1 until environment.getNodeCount).toList

    val rescuerNodes = generateUniqueRandomSequence(1, environment.getNodeCount - 1, Math.ceil(environment.getNodeCount * 0.10).toInt).toList
    val userNodes = candidate.diff(rescuerNodes)

    userNodes.foreach(environment.getNodeByID(_).setConcentration(isUser, true.asInstanceOf[T]))
    rescuerNodes.foreach(environment.getNodeByID(_).setConcentration(isRescuer, true.asInstanceOf[T]))

    val needsIntervention = 2
    val nodesRequiringIntervention = fisherYatesShuffle(userNodes).take(needsIntervention)
    nodesRequiringIntervention.zipWithIndex.foreach { case (userNode, index) =>
      val interventionTime = index * (terminationTime / needsIntervention) + 10
      environment.getNodeByID(userNode).setConcentration(new SimpleMolecule("interventionTime"), interventionTime.asInstanceOf[T])
    }

    // Setup "movementTarget" to the local position of each node
    environment.getNodes.stream().forEach { node =>
      val position = environment.getPosition(node)
      node.setConcentration(new SimpleMolecule("movementTarget"), position.asInstanceOf[T])
    }
  }

  private def generateUniqueRandomSequence(min: Int, max: Int, length: Int): Seq[Int] = {
    require(length <= max - min + 1, "Length of sequence cannot be greater than range of numbers")

    val set = scala.collection.mutable.Set[Int]()
    val result = collection.mutable.ArrayBuffer[Int]()

    while (result.length < length) {
      val nextRandom = randomGenerator.nextInt(max - min + 1) + min
      if (!set.contains(nextRandom)) {
        set.add(nextRandom)
        result += nextRandom
      }
    }
    result.toSeq
  }

  private def fisherYatesShuffle[TT](arr: List[TT]): List[TT] = {
    val array = arr.toBuffer
    for (i <- array.indices.reverse) {
      val j = randomGenerator.nextInt(i + 1)
      val temp = array(j)
      array(j) = array(i)
      array(i) = temp
    }
    array.toList
  }
}
