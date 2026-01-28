package proofPlayground
package tree

/** A zipper for [[Tree]]s.
  * 
  * This is the derivative of the type constructor [[Tree]] with respect to `A`.
  * 
  * @tparam A the type of values stored in the tree.
  */
case class TreeZipper[+A](subtree: Tree[A], context: List[TreeContext[A]])

object TreeZipper:
  def apply[A](tree: Tree[A]): TreeZipper[A] = TreeZipper(tree, Nil)

  given TreeZipper is Zipper[Tree]:
    extension [A](self: TreeZipper[A])
      override def get: Tree[A] = self.subtree

      override def down: Option[TreeZipper[A]] =
        first

      override def up: Option[TreeZipper[A]] =
        self.context match
          case Nil =>
            None
          case TreeContext(value, direction, siblings) :: rest =>
            val node = Tree(value, siblings.patch(direction, List(self.subtree), 0))
            Some(TreeZipper(node, rest))

      override def left: Option[TreeZipper[A]] =
        self.context match
          case Nil =>
            None
          case TreeContext(value, direction, siblings) :: rest =>
            if direction > 0 then
              self.up.flatMap(_.nth(direction - 1))
            else
              self.up.flatMap(_.left).flatMap(_.last)

      override def right: Option[TreeZipper[A]] =
        self.context match
          case Nil =>
            None
          case TreeContext(value, direction, siblings) :: rest =>
            if direction < siblings.length then
              self.up.flatMap(_.nth(direction + 1))
            else
              self.up.flatMap(_.right).flatMap(_.first)

      private def first: Option[TreeZipper[A]] = nth(0)

      private def last: Option[TreeZipper[A]] = nth(self.subtree.children.length - 1)

      private def nth(direction: Int): Option[TreeZipper[A]] =
        if self.subtree.isLeaf then None
        else
          if direction < 0 || direction >= self.subtree.children.length then None
          else
            val subcontext = TreeContext(self.subtree.value, direction, self.subtree.children.patch(direction, Nil, 1))
            Some(TreeZipper(self.subtree.children(direction), subcontext :: self.context))
