package proofPlayground
package tree

/** A typeclass for zippers.
  *
  * @tparam T the type of the underlying structure to be zipped.
  *         This should correspond to the antiderivative of `Self`.
  */
trait Zipper[T[_]]:
  /** The type of zippers for the underlying structure `T`. */
  type Self[_]

  extension [A](self: Self[A])
    /** Gets the underlying structure at the current position.
      *
      * @return the underlying structure at the current position.
      */
    def get: T[A]

    /** Moves the zipper one step up.
      *
      * @return the zipper at the parent position, if possible, or None otherwise.
      */
    def up: Option[Self[A]]

    /** Moves the zipper one step down.
      *
      * @return the zipper at the first child position, if possible, or None otherwise.
      */
    def down: Option[Self[A]]

    /** Moves the zipper to the left sibling.
      *
      * @return the zipper at the left sibling, if possible, or None otherwise.
      */
    def left: Option[Self[A]]

    /** Moves the zipper to the right sibling.
      *
      * @return the zipper at the right sibling, if possible, or None otherwise.
      */
    def right: Option[Self[A]]

object Zipper:
  extension [T[_]: Zipper, A](zipper: T.Self[A])
    /** Gets the root of the underlying structure reachable from the current position.
      *
      * A root is a node that has no parent.
      *
      * @return the root of the underlying structure.
      */
    @scala.annotation.tailrec
    def root: T[A] =
      zipper.up match
        case None         => zipper.get
        case Some(parent) => parent.root
