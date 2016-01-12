/*
 * Adapted from shapeless-contrib scalaz
 * https://github.com/typelevel/shapeless-contrib/blob/v0.4/scalaz/main/scala/sequence.scala
 *
 */
package cats.sequence

import shapeless._

import cats._

import scala.annotation.implicitNotFound

trait Apply2[FH, OutT] {
  type Out
  def apply(fh: FH, outT: OutT): Out
}

object Apply2 {
  type Aux[FH, OutT, Out0] = Apply2[FH, OutT] { type Out = Out0 }

  implicit def apply2[F[_], H, T <: HList](implicit app: Apply[F]): Aux[F[H], F[T], F[H :: T]] =
    new Apply2[F[H], F[T]] {
      type Out = F[H :: T]
      def apply(fh: F[H], outT: F[T]): Out = app.ap(outT)(app.map(fh)(( (_:H) :: (_:T)).curried))
    }

  implicit def apply2a[F[_, _], A, H, T <: HList](implicit app: Apply[F[A, ?]]): Aux[F[A, H], F[A, T], F[A, H :: T]] =
    apply2[F[A, ?], H, T]
}

@implicitNotFound("cannot construct sequencer, make sure that every item of you hlist ${L} is an Applicative")
trait Sequencer[L <: HList] {
  type Out
  def apply(in: L): Out
}

trait LowPrioritySequencer {
  type Aux[L <: HList, Out0] = Sequencer[L] { type Out = Out0 }

  implicit def consSequencerAux[FH, FT <: HList, OutT]
  (implicit
   st: Aux[FT, OutT],
   ap: Apply2[FH, OutT]
  ): Aux[FH :: FT, ap.Out] =
    new Sequencer[FH :: FT] {
      type Out = ap.Out
      def apply(in: FH :: FT): Out = ap(in.head, st(in.tail))
    }
}

object Sequencer extends LowPrioritySequencer {
  implicit def nilSequencerAux[F[_]: Applicative]: Aux[HNil, F[HNil]] =
    new Sequencer[HNil] {
      type Out = F[HNil]
      def apply(in: HNil): F[HNil] = Applicative[F].pure(HNil: HNil)
    }

  implicit def singleSequencerAux[FH]
  (implicit
   un: Unapply[Functor, FH]
  ): Aux[FH :: HNil, un.M[un.A :: HNil]] =
    new Sequencer[FH :: HNil] {
      type Out = un.M[un.A :: HNil]
      def apply(in: FH :: HNil): Out = un.TC.map(un.subst(in.head)) { _ :: HNil }
    }
}

trait SequenceOps {
  implicit class sequenceFunction[L <: HList](self: L) {
    def sequence(implicit seq: Sequencer[L]): seq.Out = seq(self)
  }

  object sequence extends ProductArgs {
    def applyProduct[L <: HList](l: L)(implicit seq: Sequencer[L]): seq.Out = seq(l)
  }
}

object SequenceOps extends SequenceOps
