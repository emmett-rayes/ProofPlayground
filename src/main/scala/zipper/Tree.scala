package proofPlayground
package zipper

import core.Functor

/** A recursive tree data structure with an arbitrary number of children.
  *
  * A leaf node has no children.
  *
  * @note This definition contains the sum type implicitly.
  *       The corresponding implicit functor of the least fixed-point is X => A + List[X].
  * @tparam A the type of values stored in the tree.
  */
case class Tree[+A](value: A, children: List[Tree[A]] = List.empty) {

  /** Checks if this tree is a leaf node.
    *
    * @return true if this tree is a leaf node, false otherwise.
    */
  def isLeaf: Boolean = children.isEmpty
}

object Tree {

  /** [[Functor]] instance for [[Tree]]. */
  given Functor[Tree] {
    extension [A](fa: Tree[A]) {
      def map[B](f: A => B): Tree[B] =
        Tree(f(fa.value), fa.children.map(_.map(f)))
    }
  }
}
