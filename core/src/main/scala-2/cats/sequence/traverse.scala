/*
 * Originally adapted from shapeless-contrib scalaz
 * https://github.com/typelevel/shapeless-contrib/blob/v0.4/scalaz/main/scala/traverse.scala
 *
 */
package cats.sequence

import cats.{Applicative, Parallel}
import shapeless._
import shapeless.ops.hlist._

sealed trait Traverser[F[_], L <: HList, P] extends Serializable {
  type LOut <: HList
  type Out = F[LOut]

  def apply(hl: L): Out
  def parApply(hl: L)(implicit par: Parallel[F]): Out
}

sealed abstract private[sequence] class TraverserFromSequencer {
  type Aux[F[_], L <: HList, P, O <: HList] = Traverser[F, L, P] {
    type LOut = O
  }

  implicit def fromSequencer[L <: HList, P, S <: HList](implicit
      map: Mapper.Aux[P, L, S],
      seq: Sequencer[S]
  ): Aux[seq.F, L, P, seq.LOut] = new Traverser[seq.F, L, P] {
    type LOut = seq.LOut
    def apply(hl: L) = seq(map(hl))
    def parApply(hl: L)(implicit par: Parallel[seq.F]) = seq.parApply(map(hl))(par)
  }
}

object Traverser extends TraverserFromSequencer {
  def apply[F[_], L <: HList, P](implicit trav: Traverser[F, L, P]): trav.type = trav

  implicit def nil[F[_], P](implicit app: Applicative[F]): Aux[F, HNil, P, HNil] =
    new Traverser[F, HNil, P] {
      type LOut = HNil
      def apply(hl: HNil) = app.pure(hl)
      def parApply(hl: HNil)(implicit par: Parallel[F]) =
        par.sequential(par.applicative.pure(hl))
    }
}

trait TraverseOps {
  implicit def traverseSyntax[L <: HList](hl: L): TraverseOps.Syntax[L] =
    new TraverseOps.Syntax(hl)

  def traverse[F[_], L <: HList](hl: L)(f: Poly)(implicit
      trav: Traverser[F, L, f.type]
  ): trav.Out = trav(hl)

  def parTraverse[F[_], L <: HList](hl: L)(f: Poly)(implicit
      trav: Traverser[F, L, f.type],
      par: Parallel[F]
  ): trav.Out = trav.parApply(hl)
}

object TraverseOps {
  class Syntax[L <: HList](private val self: L) extends AnyVal {
    def traverse[F[_]](f: Poly)(implicit
        trav: Traverser[F, L, f.type]
    ): trav.Out = trav(self)

    def parTraverse[F[_]](f: Poly)(implicit
        trav: Traverser[F, L, f.type],
        par: Parallel[F]
    ): trav.Out = trav.parApply(self)
  }
}
