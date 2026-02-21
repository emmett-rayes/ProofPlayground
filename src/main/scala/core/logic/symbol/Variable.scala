package proofPlayground
package core.logic.symbol

import scala.collection.mutable
import scala.reflect.ClassTag

/** Represents a typed logical variable with a unique identifier.
  *
  * @tparam K The type associated with this variable.
  * @param id The unique identifier for this variable within its type.
  */
final class Variable[K] private (val id: String) {
  override def toString: String = s"Variable($id)"

  override def hashCode(): Int = (getClass, id).hashCode()
}

case object Variable {
  private val registry = mutable.Map[(Class[?], String), Variable[?]]()

  /** Creates or gets the unique variable for the given type and identifier.
    *
    * @param id The unique identifier for the variable.
    * @tparam K The type associated with this variable. It represents the kind over which variables range.
    * @return The variable associated with the unique identifier for type K.
    */
  def apply[K: ClassTag](id: String): Variable[K] = {
    val cls = summon[ClassTag[K]].runtimeClass
    // downcast safety: the variable is freshly created with type K
    registry.getOrElseUpdate((cls, id), new Variable[K](id)).asInstanceOf[Variable[K]]
  }
}
