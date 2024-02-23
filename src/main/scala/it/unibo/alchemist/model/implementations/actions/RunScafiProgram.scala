/*
 * Copyright (C) 2010-2019, Danilo Pianini and contributors listed in the main project's alchemist/build.gradle file.
 *
 * This file is part of Alchemist, and is distributed under the terms of the
 * GNU General Public License, with a linking exception,
 * as described in the file LICENSE in the Alchemist distribution's top directory.
 */
package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.actions.AbstractLocalAction
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

sealed class RunScafiProgram[T, P <: Position[P]](
  environment: Environment[T, P],
  node: Node[T],
  reaction: Reaction[T],
  randomGenerator: RandomGenerator,
  programName: String,
  retentionTime: Double
) extends AbstractLocalAction[T](node) {

  def this(
    environment: Environment[T, P],
    node: Node[T],
    reaction: Reaction[T],
    randomGenerator: RandomGenerator,
    programName: String
  ) = this(environment, node, reaction, randomGenerator, programName, FastMath.nextUp(1.0 / reaction.getTimeDistribution.getRate))

  import RunScafiProgram.NeighborData
  val program =
    ResourceLoader.classForName(programName).getDeclaredConstructor().newInstance().asInstanceOf[CONTEXT => EXPORT]
  val programNameMolecule = new SimpleMolecule(programName)
  lazy val nodeManager = new SimpleNodeManager(node)
  private var neighborhoodManager: Map[ID, NeighborData[P]] = Map()
  private val commonNames = new ScafiIncarnationForAlchemist.StandardSensorNames {}
  private var completed = false
  declareDependencyTo(Dependency.EVERY_MOLECULE)

  def asMolecule = programNameMolecule

  val offloadingMapping: Map[(String, Int), Int] = node.getConcentration(new SimpleMolecule("offloadingMapping"))
    .asInstanceOf[Iterable[((String, Int), Int)]]
    .toMap
  val isSurrogateForSomeProgram = offloadingMapping.values.exists(_ == node.getId)
  val isSurrogateForThisProgram = offloadingMapping.exists(kv => kv._1._1 == programName && kv._2 == node.getId)
  val shouldExecuteThisProgram = !offloadingMapping
    .exists { case ((program, source), _) => source == node.getId && program == programName }

  private var contextsMap: Map[ID, CONTEXT] = Map()
  private var surrogateComputedResult: Map[ID, NeighborData[P]] = Map()
  // This optional is fulfilled with the surrogate node id where the program has been forwarded, otherwise it is empty
  private val surrogateId = offloadingMapping
    .filter { case ((program, source), _) => source == node.getId && program == programName }
    .map { case (_, surrogate) => surrogate}
    .collectFirst { case id => id }

  // This set contains the nodes that have forwarded the program to this node
  val surrogateForNodes = offloadingMapping
    .filter { case ((_, _), destination) => destination == node.getId }
    .keys
    .map { case (_, source) => source}
    .toSet

  override def cloneAction(node: Node[T], reaction: Reaction[T]) =
    new RunScafiProgram(environment, node, reaction, randomGenerator, programName, retentionTime)

  override def execute(): Unit = {
//    println(s"[${environment.getSimulation.getTime}] Node ${node.getId} - Executing program $programName")
    import scala.jdk.CollectionConverters._
    implicit def euclideanToPoint(point: P): Point3D = point.getDimensions match {
      case 1 => Point3D(point.getCoordinate(0), 0, 0)
      case 2 => Point3D(point.getCoordinate(0), point.getCoordinate(1), 0)
      case 3 => Point3D(point.getCoordinate(0), point.getCoordinate(1), point.getCoordinate(2))
    }
    val position: P = environment.getPosition(node)
    // NB: We assume it.unibo.alchemist.model.Time = DoubleTime
    //     and that its "time unit" is seconds, and then we get NANOSECONDS
    val alchemistCurrentTime = Try(environment.getSimulation)
      .map(_.getTime)
      .orElse(
        Failure(new IllegalStateException("The simulation is uninitialized (did you serialize the environment?)"))
      )
      .get
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
    val localSensors = node.getContents().asScala.map { case (k, v) => k.getName -> v }

    val neighborhoodSensors = scala.collection.mutable.Map[CNAME, Map[ID, Any]]()
    val exports: Iterable[(ID, EXPORT)] = neighborhoodManager.view.mapValues(_.exportData)
    val context = new ContextImpl(node.getId, exports, localSensors, Map.empty) {
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
          val k = position.getDimensions()
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

    if (!shouldExecuteThisProgram) {
      // This node has forwarded the module to another node
      // This node should provide the context to the surrogate node to execute the program
      val surrogateNode = surrogateId match {
        case Some(id) => environment.getNodeByID(id)
        case None => throw new IllegalStateException(s"The Node ${node.getId} should have forwarded the program to another node")
      }
      // Set into the surrogate node the context of the original node
      getScafiProgramFromNode(surrogateNode).foreach { action => action.setContextFor(node.getId, context) }
      completed = true
      return
    }

    if (isSurrogateForThisProgram) {
      // Surrogate node must execute the program for all the nodes that have forwarded the program to it
      surrogateForNodes.foreach(deviceId => {
        // If the context for the node is available, execute the program, otherwise skip to the next cycle
        contextsMap.get(deviceId).foreach(contextForNode => {
          val computedResult = program(contextForNode)
          val originalNodePosition = environment.getPosition(environment.getNodeByID(deviceId))
          val toSend = NeighborData(computedResult, originalNodePosition, alchemistCurrentTime)
          surrogateComputedResult = surrogateComputedResult + (deviceId -> toSend)
        })
      })
      completed = true
      return
    }

    val computed = program(context)
    node.setConcentration(programName, computed.root[T]())
    val toSend = NeighborData(computed, position, alchemistCurrentTime)
    neighborhoodManager = neighborhoodManager + (node.getId -> toSend)
    completed = true
  }

  def getScafiProgramFromNode(node: Node[T]): Option[RunScafiProgram[T, P]] =
    ScafiIncarnationUtils.allScafiProgramsFor[T, P](environment.getNodeByID(node.getId))
      .filter(this.getClass.isInstance(_))
      .collectFirst { case action if action.programNameMolecule == programNameMolecule => action }

  def sendExport(id: ID, exportData: NeighborData[P]): Unit = neighborhoodManager += id -> exportData

  def getExport(id: ID): Option[NeighborData[P]] = neighborhoodManager.get(id)

  def setContextFor(id: ID, context: CONTEXT): Unit = contextsMap += id -> context

  def getResultFor(id: ID): Option[NeighborData[P]] = surrogateComputedResult.get(id)

  def isComputationalCycleComplete: Boolean = completed

  def prepareForComputationalCycle: Unit = completed = false

}

object RunScafiProgram {
  case class NeighborData[P <: Position[P]](exportData: EXPORT, position: P, executionTime: AlchemistTime)

  implicit class RichMap[K, V](map: Map[K, V]) {
    def mapValuesStrict[T](f: V => T): Map[K, T] = map.map(tp => tp._1 -> f(tp._2))
  }
}