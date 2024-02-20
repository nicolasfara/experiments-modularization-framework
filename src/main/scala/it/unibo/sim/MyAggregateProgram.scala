package it.unibo.sim

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

abstract class MyAggregateProgram
  extends AggregateProgram with 
  StandardSensors with ScafiAlchemistSupport with BlockG with BlockC with Gradients with FieldUtils {

    def senseOr[T](name: String, otherwise: T): T = if(node.has(name)) sense[T](name) else otherwise
}
