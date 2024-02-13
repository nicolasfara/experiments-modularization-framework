package it.unibo.sim

import it.unibo.alchemist.model.molecules.SimpleMolecule
import it.unibo.alchemist.model.{Environment, Position}
import scala.jdk.OptionConverters._

def infrastructureNeighbourSend[Payload](key: String, payload: Payload, hostId: Int)(implicit env: Environment[Any, Position[_]]): Payload = {
  val currentNode = env.getNodes.stream().filter { n => n.getId == hostId }.findFirst().get()
  env.getNeighborhood(currentNode).forEach { n =>
    n.setConcentration(new SimpleMolecule(key), payload)
  }
  payload
}

def infrastructureNeighbourReceive[Payload](key: String, hostId: Int)(implicit env: Environment[Any, Position[_]]): Option[Payload] =
  env.getNodes.stream().filter { n => n.getId == hostId }.findFirst().map { n =>
    n.getConcentration(new SimpleMolecule(key)).asInstanceOf[Payload]
  }.toScala
