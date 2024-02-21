/*
 * Copyright (C) 2010-2019, Danilo Pianini and contributors
 * listed in the main project's alchemist/build.gradle.kts file.
 *
 * This file is part of Alchemist, and is distributed under the terms of the
 * GNU General Public License, with a linking exception,
 * as described in the file LICENSE in the Alchemist distribution's top directory.
 */

package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.{Node, Position, Reaction, ScafiIncarnationUtils}
import it.unibo.alchemist.model.ScafiIncarnationUtils._
import it.unibo.alchemist.model.implementations.nodes.ScafiDevice
import it.unibo.alchemist.model._
import it.unibo.alchemist.model.actions.AbstractAction
import it.unibo.alchemist.model.molecules.SimpleMolecule

import java.util.stream.Collectors
import scala.jdk.CollectionConverters._

class SendScafiMessage[T, P <: Position[P]](
  environment: Environment[T, P],
  device: ScafiDevice[T],
  reaction: Reaction[T],
  val program: RunScafiProgram[T, P]
) extends AbstractAction[T](device.getNode) {
  assert(reaction != null, "Reaction cannot be null")
  assert(program != null, "Program cannot be null")

  /**
   * This method allows to clone this action on a new node. It may result useful to support runtime creation of nodes
   * with the same reaction programming, e.g. for morphogenesis.
   *
   * @param destinationNode
   *   The node where to clone this { @link Action}
   * @param reaction
   *   The reaction to which the CURRENT action is assigned
   * @return
   *   the cloned action
   */
  override def cloneAction(destinationNode: Node[T], reaction: Reaction[T]): Action[T] =
    runInScafiDeviceContext[T, Action[T]](
      node = destinationNode,
      message =
        getClass.getSimpleName + " cannot get cloned on a node of type " + destinationNode.getClass.getSimpleName,
      device => {
        val possibleRef = destinationNode.getReactions
          .stream()
          .flatMap(reaction => reaction.getActions.stream())
          .filter(action => action.isInstanceOf[RunScafiProgram[_, _]])
          .map(action => action.asInstanceOf[RunScafiProgram[T, P]])
          .collect(Collectors.toList[RunScafiProgram[T, P]])
        if (possibleRef.size() == 1) {
          return new SendScafiMessage(environment, device, reaction, possibleRef.get(0))
        }
        throw new IllegalStateException(
          "There must be one and one only unconfigured " + RunScafiProgram.getClass.getSimpleName
        )
      }
    )

  /** Effectively executes this action. */
  override def execute(): Unit = {
    val messagesMolecule = new SimpleMolecule("messagesCount")
    if (program.isSurrogate) {
      // Skip the send if it is a surrogate
      program.prepareForComputationalCycle
      return
    }
    if (!program.shouldExecuteThisProgram) {
      // Get program from surrogate
      val surrogateNode = getSurrogateNode(program)
      getSurrogateProgramFromNode(surrogateNode).foreach(surrogateProgram => {
        // Write the result of the surrogate program to the node requesting the result
        surrogateProgram.getResultFor(device.getNode.getId) match {
          case Some(computedResult) =>
            device.getNode().setConcentration(program.programNameMolecule, computedResult.exportData.root())
            // Send the result to the neighbors of the device requesting the result
            getNeighborProgramsFromNode(device.getNode).foreach(action => {
              action.sendExport(device.getNode.getId, computedResult)
            })
            // Send to self the result
            program.sendExport(device.getNode.getId, computedResult)
            // Set the number of messages sent
            val messages = environment.getNeighborhood(device.getNode()).size() + 2 // +2 because of the communication with the surrogate
            device.getNode().setConcentration(messagesMolecule, messages.asInstanceOf[T])
          case _ => () // Skip the message sending if the surrogate has not computed the result yet
        }
      })
      program.prepareForComputationalCycle
      return
    }

    // ----------------- ORIGINAL CODE -----------------
    val toSend = program.getExport(device.getNode.getId).get
    getNeighborProgramsFromNode(device.getNode).foreach(action => {
      action.sendExport(device.getNode.getId, toSend)
    })
    program.prepareForComputationalCycle
    val messages = environment.getNeighborhood(device.getNode()).size()
    device.getNode().setConcentration(messagesMolecule, messages.asInstanceOf[T])
  }

  private def getSurrogateNode(program: RunScafiProgram[T, P]): Node[T] = {
    val surrogateId = program.offloadingMapping.filter { case ((programName, source), _) =>
      source == device.getNode.getId && programName == program.programNameMolecule.getName
    }.values.head
    environment.getNodeByID(surrogateId)
  }

  private def getSurrogateProgramFromNode(surrogateNode: Node[T]): Option[RunScafiProgram[T, P]] =
    ScafiIncarnationUtils.allScafiProgramsFor[T, P](surrogateNode).filter(program.getClass.isInstance(_))
      .collectFirst { case action if action.programNameMolecule == program.programNameMolecule => action }

  private def getNeighborProgramsFromNode(node: Node[T]): Iterator[RunScafiProgram[T, P]] =
    for {
      neighborhood <- environment.getNeighborhood(node).getNeighbors.iterator().asScala
      action <- ScafiIncarnationUtils.allScafiProgramsFor[T, P](neighborhood).filter(program.getClass.isInstance(_))
      if action.programNameMolecule == program.programNameMolecule
    } yield action

  /** @return The context for this action. */
  override def getContext: Context = Context.NEIGHBORHOOD
}