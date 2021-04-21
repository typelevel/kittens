/*
 * Originally adapted from shapeless-contrib scalaz
 * https://github.com/typelevel/shapeless-contrib/blob/v0.4/scalaz/main/scala/traverse.scala
 *
 */
package cats.sequence

import cats.Parallel
import shapeless._
import shapeless.ops.hlist._

sealed trait Traverser[L <: HList, P] extends Serializable {
  type F[_]
  type LOut <: HList
  type Out = F[LOut]
  def apply(hl: L): Out

  // Defined sequentially for binary compatibility, overridden by implementations.
  protected[sequence] def par(hl: L)(implicit P: Parallel[F]): P.F[LOut] =
    P.parallel(apply(hl))

  final def parApply(hl: L)(implicit P: Parallel[F]): Out =
    P.sequential(par(hl)(P))
}

object Traverser {
  type Aux[L <: HList, P, F0[_], LOut0 <: HList] = Traverser[L, P] {
    type F[X] = F0[X]
    type LOut = LOut0
  }

  implicit def mkTraverser[L <: HList, P, S <: HList](implicit
      mapper: Mapper.Aux[P, L, S],
      sequencer: Sequencer[S]
  ): Aux[L, P, sequencer.F, sequencer.LOut] = new Traverser[L, P] {
    type F[X] = sequencer.F[X]
    type LOut = sequencer.LOut
    def apply(hl: L) = sequencer(mapper(hl))
    override def par(hl: L)(implicit P: Parallel[F]) = sequencer.par(mapper(hl))(P)
  }
}

trait TraverseFunctions {
  def traverse[L <: HList](hl: L)(f: Poly)(implicit traverser: Traverser[L, f.type]): traverser.Out =
    traverser(hl)

  def parTraverse[F[_], L <: HList, LOut <: HList](hl: L)(f: Poly)(implicit
      traverser: Traverser.Aux[L, f.type, F, LOut],
      par: Parallel[F]
  ): F[LOut] = traverser.parApply(hl)
}

trait TraverseOps extends {
  implicit class withTraverse[L <: HList](self: L) {
    def traverse(f: Poly)(implicit traverser: Traverser[L, f.type]): traverser.Out =
      traverser(self)

    def parTraverse[F[_], LOut <: HList](f: Poly)(implicit
        traverser: Traverser.Aux[L, f.type, F, LOut],
        par: Parallel[F]
    ): F[LOut] = traverser.parApply(self)
  }
}
