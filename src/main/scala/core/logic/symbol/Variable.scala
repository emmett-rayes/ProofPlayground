package proofPlayground
package core.logic.symbol

import scala.collection.mutable
import scala.reflect.ClassTag

/** Companion object for creating unique variables */
case object Variable:
  private val counters = mutable.Map[Class[?], Int]().withDefaultValue(0)

  /** Creates a new unique variable for the given type
    * @tparam K The type associated with this variable.
    *         It represents the kind over which variables range.
    * @return A new variable with a unique Identifier for type K
    */
  def apply[K: ClassTag](): Variable[K] =
    val cls = summon[ClassTag[K]].runtimeClass
    val id = counters(cls)
    counters(cls) = id + 1
    new Variable[K](id)

/** Represents a typed logical variable with a unique identifier
  * @tparam K The type associated with this variable
  * @param id The unique identifier for this variable within its type
  */
case class Variable[K](id: Int)
