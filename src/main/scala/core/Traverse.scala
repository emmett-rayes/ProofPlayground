package proofPlayground
package core

object Traverse:
  extension [A](seq: Seq[A])
    def traverse[B](f: A => Option[B]): Option[Seq[B]] =
      seq.foldLeft(Option(Seq.empty[B])) { (acc, a) =>
        for
          b  <- f(a)
          bs <- acc
        yield b +: bs
      }
