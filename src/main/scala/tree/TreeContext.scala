package proofPlayground
package tree

/** A one-hole context for the type constructor [[Tree]].
  *
  * This represents the derivative of [[Tree]] with respect to `A`.
  *
  * @tparam A the type of values stored in the tree.
  */
case class TreeContext[+A](value: A, direction: Int, siblings: List[Tree[A]])
