/*
 * Copyright (C) 2010-2019, Danilo Pianini and contributors listed in the main project's alchemist/build.gradle file.
 *
 * This file is part of Alchemist, and is distributed under the terms of the
 * GNU General Public License, with a linking exception,
 * as described in the file LICENSE in the Alchemist distribution's top directory.
 */
package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.actions.AbstractLocalAction
import it.unibo.alchemist.model.implementations.actions.RunScafiProgram.getAlchemistCurrentTime
import it.unibo.alchemist.model.{Node, Position, Reaction}
import it.unibo.alchemist.model.implementations.nodes.SimpleNodeManager
import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.{Time => AlchemistTime, _}
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist.{ContextImpl, _}
import it.unibo.alchemist.scala.PimpMyAlchemist._
import it.unibo.scafi.space.Point3D
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.util.FastMath
import org.kaikikm.threadresloader.ResourceLoader

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Try}
import scala.jdk.CollectionConverters.{CollectionHasAsScala, IterableHasAsScala, ListHasAsScala, _}
import scala.language.implicitConversions

sealed class DefaultRunScafiProgram[P <: Position[P]](
    environment: Environment[Any, P],
    node: Node[Any],
    reaction: Reaction[Any],
    randomGenerator: RandomGenerator,
    programName: String,
    retentionTime: Double,
    surrogateOf: ID,
    forwardNode: ID
) extends RunScafiProgram[Any, P](environment, node, reaction, randomGenerator, programName, retentionTime, surrogateOf, forwardNode) {

  def this(
      environment: Environment[Any, P],
      node: Node[Any],
      reaction: Reaction[Any],
      randomGenerator: RandomGenerator,
      programName: String
  ) = {
    this(
      environment,
      node,
      reaction,
      randomGenerator,
      programName,
      FastMath.nextUp(1.0 / reaction.getTimeDistribution.getRate),
      node.getId(),
      node.getId(),
    )
  }
}

sealed class RunScafiProgram[T, P <: Position[P]](
    environment: Environment[T, P],
    node: Node[T],
    reaction: Reaction[T],
    randomGenerator: RandomGenerator,
    val programName: String,
    retentionTime: Double,
    surrogateOfDevice: ID,
    val forwardNode: ID,
) extends AbstractLocalAction[T](node) {

  val surrogateOf = if (surrogateOfDevice >= 0) surrogateOfDevice else node.getId()

  /*

  def this(
      environment: Environment[T, P],
      node: Node[T],
      reaction: Reaction[T],
      randomGenerator: RandomGenerator,
      programName: String
  ) = {
    this(
      environment,
      node,
      reaction,
      randomGenerator,
      programName,
      FastMath.nextUp(1.0 / reaction.getTimeDistribution.getRate),
      node.getId(),
      List.empty
    )
  }
   */

  import RunScafiProgram.NeighborData
  val program =
    ResourceLoader.classForName(programName).getDeclaredConstructor().newInstance().asInstanceOf[CONTEXT => EXPORT]
  val programNameMolecule = new SimpleMolecule(programName)
  val referenceNode = if(node.getId == surrogateOf) node else environment.getNodeByID(surrogateOf)
  println(s"Node ${node.getId} is a surrogate of ${referenceNode.getId} (same as $surrogateOf) concerning $programName")
  private lazy val nodeManager = new SimpleNodeManager(node) // node manager provides access to local node data
  private var neighborhoodManager: Map[ID, NeighborData[P]] = Map()
  private val commonNames = new ScafiIncarnationForAlchemist.StandardSensorNames {}
  private var completed = false
  declareDependencyTo(Dependency.EVERY_MOLECULE)

  // We assume all the nodes have the application dependency graph, capturing how modules are related
  private val dependencyGraph = nodeManager.getOrElse[Map[String,List[String]]]("dependencyGraph", Map.empty)

  def asMolecule: SimpleMolecule = programNameMolecule

  override def cloneAction(node: Node[T], reaction: Reaction[T]) =
    new RunScafiProgram(environment, node, reaction, randomGenerator, programName, retentionTime, surrogateOf, forwardNode)

  var forwardTo: Option[Node[T]] = None
  var surrogateExport: Option[(ID, EXPORT)] = None

  private def computeNodesReceivingProgramResult(node: Node[T], dependencies: List[String]): List[Node[T]] = {
    val dependencies = dependencyGraph.getOrElse(programName, List.empty)
    for {
      physicalNeighbour <- environment.getNeighborhood(node).getNeighbors.listIterator().asScala.toList
      _ <- ScafiIncarnationUtils.allScafiProgramsFor[T, P](physicalNeighbour)
        .map(p => (p.programName, p.surrogateOf)).filter(_._2 == node.getId).map(_._1).intersect(dependencies)
      if physicalNeighbour.getId != node.getId
    } yield physicalNeighbour
  }

  override def execute(): Unit = {
    val isSurrogate = referenceNode != node
    val dependenciesModules = dependencyGraph.getOrElse(programName, List.empty)
    val nodesRequiringResult = computeNodesReceivingProgramResult(node, dependenciesModules)

    val context = if (!isSurrogate) getContextForLocalNode else contextForSurrogateNode(referenceNode)
    if (forwardNode == -1) {
      val computedExport = program(context)
      val computedResult = computedExport.root[T]()

      // Write the result to dependent modules
      if (isSurrogate) referenceNode.setConcentration(programName, computedResult)

      if (isSurrogate) {
        // Set the computed export
        surrogateExport = Some(referenceNode.getId, computedExport)
      } else {
        // If I am not supporting a program, I have to compute the export
        val toSend = NeighborData(computedExport, environment.getPosition(node), getAlchemistCurrentTime(environment))
        neighborhoodManager = neighborhoodManager + (node.getId -> toSend)
      }

      //    if(referenceNode != node) {
      //      println(s"Node ${node.getId} is a surrogate of ${referenceNode.getId} and so has to forward to it")
      //      forwardTo = Some(referenceNode)
      //    }
      //    val dependencies = dependencyGraph.getOrElse(programName, List.empty)
      //    if(referenceNode == node) {
      //      // no offloading: but may need to forward to offloaded programs
      //      for {
      //        physicalNbr <- environment.getNeighborhood(node).getNeighbors.iterator().asScala
      //        action <- ScafiIncarnationUtils.allScafiProgramsFor[T, P](physicalNbr)
      //          .map(p => (p.programName, p.surrogateOf))
      //          .filter(_._2 == referenceNode.getId)
      //          .map(_._1)
      //          .intersect(dependencies)
      //        if physicalNbr.getId != node.getId
      //      } {
      //        println(s"Node ${node.getId} is running $programName and has to forward to ${physicalNbr.getId} to support $action")
      //        forwardTo = Some(physicalNbr)
      //      }
      //    } else {
      //      // offloading: needs to forward to the reference node
      //      println(s"Surrogate node ${node.getId} is running $programName and has to forward to the original node ${referenceNode.getId}")
      //      forwardTo = Some(referenceNode)
      //    }
      //
      //    val context = if (referenceNode == node) getContextForLocalNode else contextForSurrogateNode(referenceNode)
      //    val computed = program(context)
      //    export = Some(computedExport)
      //    if (referenceNode == node) {
      //      node.setConcentration(programName, computedExport.root[T]())
      //    } else {
      //      environment.getNodeByID(referenceNode.getId).setConcentration(programName, computedExport.root[T]())
      //    }
      //
      //    val toSend = NeighborData(computedExport, environment.getPosition(referenceNode), getAlchemistCurrentTime(environment))
      //    neighborhoodManager = neighborhoodManager + (referenceNode.getId -> toSend)
    }
    completed = true
  }

  implicit def euclideanToPoint(point: P): Point3D = point.getDimensions match {
    case 1 => Point3D(point.getCoordinate(0), 0, 0)
    case 2 => Point3D(point.getCoordinate(0), point.getCoordinate(1), 0)
    case 3 => Point3D(point.getCoordinate(0), point.getCoordinate(1), point.getCoordinate(2))
  }

  private def contextForSurrogateNode(originalNode: Node[T]): CONTEXT = {
    val originalNodeEnv = originalNode.getReactions.asScala
      .flatMap(_.getActions.asScala)
      .collectFirst {
        case action: RunScafiProgram[T, P] if action.programNameMolecule == programNameMolecule => action
      }.getOrElse(throw new IllegalStateException(s"Surrogate node ${originalNode.getId} does not run $programName"))
    originalNodeEnv.getContextForLocalNode
  }

  def getContextForLocalNode: CONTEXT = {
    val neighborhoodSensors = scala.collection.mutable.Map[CNAME, Map[ID, Any]]()
    val exports: Iterable[(ID, EXPORT)] = neighborhoodManager.view.mapValues(_.exportData)

    import scala.jdk.CollectionConverters._

    val position: P = environment.getPosition(node)
    // NB: We assume it.unibo.alchemist.model.Time = DoubleTime
    //     and that its "time unit" is seconds, and then we get NANOSECONDS
    val alchemistCurrentTime = RunScafiProgram.getAlchemistCurrentTime(environment)
    def alchemistTimeToNanos(time: AlchemistTime): Long = (time.toDouble * 1_000_000_000).toLong
    val currentTime: Long = alchemistTimeToNanos(alchemistCurrentTime)
    if (!neighborhoodManager.contains(node.getId)) {
      neighborhoodManager += node.getId -> NeighborData(factory.emptyExport(), position, Double.NaN)
    }
    neighborhoodManager = neighborhoodManager.filter { case (id, data) =>
      id == node.getId || data.executionTime >= alchemistCurrentTime - retentionTime
    }
    val deltaTime: Long =
      currentTime - neighborhoodManager.get(node.getId).map(d => alchemistTimeToNanos(d.executionTime)).getOrElse(0L)
    val localSensors = node.getContents.asScala.map { case (k, v) => k.getName -> v }

    new ContextImpl(node.getId, exports, localSensors, Map.empty) {
      override def nbrSense[TT](nsns: CNAME)(nbr: ID): Option[TT] =
        neighborhoodSensors
          .getOrElseUpdate(
            nsns,
            nsns match {
              case commonNames.NBR_LAG =>
                neighborhoodManager.mapValuesStrict[FiniteDuration](nbr =>
                  FiniteDuration(alchemistTimeToNanos(alchemistCurrentTime - nbr.executionTime), TimeUnit.NANOSECONDS)
                )
              /*
               * nbrDelay is estimated: it should be nbr(deltaTime), here we suppose the round frequency
               * is negligibly different between devices.
               */
              case commonNames.NBR_DELAY =>
                neighborhoodManager.mapValuesStrict[FiniteDuration](nbr =>
                  FiniteDuration(
                    alchemistTimeToNanos(nbr.executionTime) + deltaTime - currentTime,
                    TimeUnit.NANOSECONDS
                  )
                )
              case commonNames.NBR_RANGE => neighborhoodManager.mapValuesStrict[Double](_.position.distanceTo(position))
              case commonNames.NBR_VECTOR =>
                neighborhoodManager.mapValuesStrict[Point3D](_.position.minus(position.getCoordinates))
              case NBR_ALCHEMIST_LAG =>
                neighborhoodManager.mapValuesStrict[Double](alchemistCurrentTime - _.executionTime)
              case NBR_ALCHEMIST_DELAY =>
                neighborhoodManager.mapValuesStrict(nbr =>
                  alchemistTimeToNanos(nbr.executionTime) + deltaTime - currentTime
                )
            }
          )
          .get(nbr)
          .map(_.asInstanceOf[TT])

      override def sense[TT](lsns: String): Option[TT] = (lsns match {
        case LSNS_ALCHEMIST_COORDINATES => Some(position.getCoordinates)
        case commonNames.LSNS_DELTA_TIME => Some(FiniteDuration(deltaTime, TimeUnit.NANOSECONDS))
        case commonNames.LSNS_POSITION =>
          val k = position.getDimensions
          Some(
            Point3D(
              position.getCoordinate(0),
              if (k >= 2) position.getCoordinate(1) else 0,
              if (k >= 3) position.getCoordinate(2) else 0
            )
          )
        case commonNames.LSNS_TIMESTAMP => Some(currentTime)
        case commonNames.LSNS_TIME => Some(java.time.Instant.ofEpochMilli((alchemistCurrentTime * 1000).toLong))
        case LSNS_ALCHEMIST_NODE_MANAGER => Some(nodeManager)
        case LSNS_ALCHEMIST_DELTA_TIME =>
          Some(
            alchemistCurrentTime.minus(
              neighborhoodManager.get(node.getId).map(_.executionTime).getOrElse(AlchemistTime.INFINITY)
            )
          )
        case LSNS_ALCHEMIST_ENVIRONMENT => Some(environment)
        case LSNS_ALCHEMIST_RANDOM => Some(randomGenerator)
        case LSNS_ALCHEMIST_TIMESTAMP => Some(alchemistCurrentTime)
        case _ => localSensors.get(lsns)
      }).map(_.asInstanceOf[TT])
    }
  }

  def sendExport(id: ID, exportData: NeighborData[P]): Unit = {
    println(s"Node ${node.getId} is sending export to $id concerning $programName")
    neighborhoodManager += id -> exportData
  }

  def getExport(id: ID): Option[NeighborData[P]] = neighborhoodManager.get(id)

  def isComputationalCycleComplete: Boolean = completed

  def prepareForComputationalCycle(): Unit = completed = false

}

object RunScafiProgram {
  case class NeighborData[P <: Position[P]](exportData: EXPORT, position: P, executionTime: AlchemistTime)

  implicit class RichMap[K, V](map: Map[K, V]) {
    def mapValuesStrict[T](f: V => T): Map[K, T] = map.map(tp => tp._1 -> f(tp._2))
  }

  def getAlchemistCurrentTime[T,P <: Position[P]](environment: Environment[T,P]): AlchemistTime = Try(environment.getSimulation)
    .map(_.getTime)
    .orElse(
      Failure(new IllegalStateException("The simulation is uninitialized (did you serialize the environment?)"))
    ).get
}
