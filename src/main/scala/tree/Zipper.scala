package proofPlayground
package tree

/** A type class for zippers.
  *
  * @tparam T the type of the underlying structure to be zipped.
  *         This should correspond to the antiderivative of the one-hole context type
  *         used in the implementation of the zipper.
  */
trait Zipper[T[_]]:
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
