package proofPlayground
package zipper

/** A one-hole context for the type constructor [[Tree]].
  *
  * This is the derivative of the underlying implicit functor of the recursive type [[Tree]]
  * with respect to the functor's argument (the recursive position).
  *
  * @tparam A the type of values stored in the tree.
  */
case class TreeContext[+A](value: A, direction: Int, siblings: List[Tree[A]])
