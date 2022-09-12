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

  // Defined sequentially for binary compatibility, overridden by implementations.
  protected[sequence] def par(hl: L)(implicit P: Parallel[F]): P.F[LOut] =
    P.parallel(apply(hl))

  final def parApply(hl: L)(implicit P: Parallel[F]): Out =
    P.sequential(par(hl)(P))
}

sealed abstract private[sequence] class MkTraverser {
  type Aux[F[_], L <: HList, P, O <: HList] = Traverser[F, L, P] {
    type LOut = O
  }

  implicit def mkTraverser[L <: HList, P, S <: HList](implicit
      mapper: Mapper.Aux[P, L, S],
      sequencer: Sequencer[S]
  ): Aux[sequencer.F, L, P, sequencer.LOut] = new Traverser[sequencer.F, L, P] {
    type LOut = sequencer.LOut
    def apply(hl: L) = sequencer(mapper(hl))
    override def par(hl: L)(implicit P: Parallel[sequencer.F]) = sequencer.par(mapper(hl))(P)
  }
}

object Traverser extends MkTraverser {
  implicit def hnilTraverser[F[_], P](implicit
      F: Applicative[F]
  ): Aux[F, HNil, P, HNil] = new Traverser[F, HNil, P] {
    type LOut = HNil
    def apply(nil: HNil) = F.pure(nil)
    override def par(nil: HNil)(implicit P: Parallel[F]) = P.applicative.pure(nil)
  }
}

trait TraverseFunctions {
  def traverse[F[_], L <: HList](hl: L)(f: Poly)(implicit traverser: Traverser[F, L, f.type]): traverser.Out =
    traverser(hl)

  def parTraverse[F[_], L <: HList](hl: L)(f: Poly)(implicit
      traverser: Traverser[F, L, f.type],
      par: Parallel[F]
  ): traverser.Out = traverser.parApply(hl)
}

trait TraverseOps extends {
  implicit class withTraverse[L <: HList](self: L) {
    def traverse[F[_]](f: Poly)(implicit traverser: Traverser[F, L, f.type]): traverser.Out =
      traverser(self)

    def parTraverse[F[_]](f: Poly)(implicit
        traverser: Traverser[F, L, f.type],
        par: Parallel[F]
    ): traverser.Out = traverser.parApply(self)
  }
}
