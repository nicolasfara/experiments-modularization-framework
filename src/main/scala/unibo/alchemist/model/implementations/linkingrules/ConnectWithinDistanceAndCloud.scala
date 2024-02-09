package it.unibo.alchemist.model.implementations.linkingrules;

import java.util.stream.Collectors
import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.implementations.neighborhoods.Neighborhoods
import it.unibo.alchemist.model.interfaces._

class ConnectWithinDistanceAndCloud[T, P <: Position[P]](radius: Double)
  extends ConnectWithinDistance[T, P](radius) {
  val deviceTypeMoleculeName = "nodeType"
  val centralNode = 1

  override def computeNeighborhood(center: Node[T], env: Environment[T, P]): Neighborhood[T] = {
    val nbrs = env.getNodesWithinRange(center, getRange())
    Neighborhoods.make(env, center,
      if (center.getConcentration(new SimpleMolecule(deviceTypeMoleculeName)) != centralNode) { // normal
        nbrs.addAll(env.getNodes.stream().filter(n => n.getConcentration(new SimpleMolecule(deviceTypeMoleculeName)) == centralNode)
          .collect(Collectors.toList[Node[T]]))
        nbrs
      } else if (center.getConcentration(new SimpleMolecule(deviceTypeMoleculeName)) == centralNode) { // candidate manager
        nbrs.addAll(env.getNodes.stream()
          .collect(Collectors.toList[Node[T]]))
        nbrs
      } else nbrs
    )
  }
}