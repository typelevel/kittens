/*
 * Originally adapted from shapeless-contrib scalaz
 * https://github.com/typelevel/shapeless-contrib/blob/v0.4/scalaz/main/scala/sequence.scala
 *
 */
package cats.sequence

import cats.{Apply, Invariant, InvariantMonoidal, InvariantSemigroupal, Parallel, Semigroupal}
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist.Align

import scala.annotation.implicitNotFound

@implicitNotFound("Cannot infer Sequencer for ${L}, make sure that every item is wrapped in the same Applicative")
sealed trait Sequencer[L <: HList] extends Serializable {
  type F[_]
  type LOut <: HList
  type Out = F[LOut]

  def apply(hl: L): Out
  def parApply(hl: L)(implicit par: Parallel[F]): Out
}

sealed abstract private[sequence] class SequencerForRecord {
  type Aux[G[_], L <: HList, O <: HList] = Sequencer[L] {
    type F[x] = G[x]
    type LOut = O
  }

  implicit def consField[G[_], K, V, GT <: HList, T <: HList](implicit
      isg: InvariantSemigroupal[G],
      tail: Aux[G, GT, T]
  ): Aux[G, FieldType[K, G[V]] :: GT, FieldType[K, V] :: T] = new Sequencer[FieldType[K, G[V]] :: GT] {
    type F[x] = G[x]
    type LOut = FieldType[K, V] :: T

    private def cons(v: V, t: T) = field[K][V](v) :: t
    private def tuple(ht: V :: T) = (field[K][V](ht.head), ht.tail)

    def parApply(hl: FieldType[K, F[V]] :: GT)(implicit par: Parallel[F]) =
      Parallel.parMap2(hl.head: F[V], tail.parApply(hl.tail))(cons)

    def apply(hl: FieldType[K, F[V]] :: GT) = isg match {
      case app: Apply[G] => app.map2(hl.head, tail(hl.tail))(cons)
      case _ => Semigroupal.imap2(hl.head: F[V], tail(hl.tail))(cons)(tuple)
    }
  }
}

object Sequencer extends SequencerForRecord {
  def apply[L <: HList](implicit seq: Sequencer[L]): seq.type = seq

  implicit def nil[G[_]](implicit im: InvariantMonoidal[G]): Sequencer.Aux[G, HNil, HNil] =
    new Sequencer[HNil] {
      type F[x] = G[x]
      type LOut = HNil

      def apply(hl: HNil) = im.point(hl)
      def parApply(hl: HNil)(implicit par: Parallel[F]) =
        par.sequential(par.applicative.pure(hl))
    }

  implicit def cons[G[_], H, GT <: HList, T <: HList](implicit
      isg: InvariantSemigroupal[G],
      tail: Aux[G, GT, T]
  ): Aux[G, G[H] :: GT, H :: T] = new Sequencer[G[H] :: GT] {
    type F[x] = G[x]
    type LOut = H :: T

    def parApply(hl: F[H] :: GT)(implicit par: Parallel[G]) =
      Parallel.parMap2(hl.head, tail.parApply(hl.tail))(_ :: _)

    def apply(hl: F[H] :: GT) = isg match {
      case app: Apply[F] => app.map2(hl.head, tail(hl.tail))(_ :: _)
      case _ => Semigroupal.imap2(hl.head, tail(hl.tail))(_ :: _) { case h :: t => (h, t) }
    }
  }
}

sealed trait GenericSequencer[A, L <: HList] extends Serializable {
  type F[_]

  def apply(hl: L): F[A]
  def parApply(hl: L)(implicit par: Parallel[F]): F[A]
}

sealed abstract private[sequence] class GenericSequencerForRecord {
  type Aux[G[_], A, L <: HList] = GenericSequencer[A, L] {
    type F[x] = G[x]
  }

  implicit def forRecord[G[_], A, L <: HList, R <: HList, S <: HList](implicit
      gen: LabelledGeneric.Aux[A, R],
      seq: Sequencer.Aux[G, L, S],
      inv: Invariant[G],
      alignFrom: Align[S, R],
      alignTo: Align[R, S]
  ): GenericSequencer.Aux[G, A, L] = new GenericSequencer[A, L] {
    type F[x] = G[x]

    private def from(s: S) = gen.from(s.align)
    private def to(a: A) = gen.to(a).align

    def apply(hl: L) = inv.imap(seq(hl))(from)(to)
    def parApply(hl: L)(implicit par: Parallel[G]) =
      par.monad.map(seq.parApply(hl))(from)
  }

}

object GenericSequencer extends GenericSequencerForRecord {
  def apply[A, L <: HList](implicit seq: GenericSequencer[A, L]): seq.type = seq

  implicit def forProduct[G[_], A, L <: HList, R <: HList](implicit
      gen: Generic.Aux[A, R],
      seq: Sequencer.Aux[G, L, R],
      inv: Invariant[G]
  ): GenericSequencer.Aux[G, A, L] = new GenericSequencer[A, L] {
    type F[x] = G[x]

    def apply(hl: L) =
      inv.imap(seq(hl))(gen.from(_))(gen.to(_))
    def parApply(hl: L)(implicit par: Parallel[G]) =
      par.monad.map(seq.parApply(hl))(gen.from(_))
  }
}

trait SequenceOps {
  implicit def sequenceSyntax[L <: HList](hl: L): SequenceOps.Syntax[L] =
    new SequenceOps.Syntax(hl)

  object sequence extends ProductArgs {
    def applyProduct[L <: HList](hl: L)(implicit seq: Sequencer[L]): seq.Out = seq(hl)
  }

  object parSequence extends ProductArgs {
    def applyProduct[F[_], L <: HList, O <: HList](hl: L)(implicit
        seq: Sequencer.Aux[F, L, O],
        par: Parallel[F]
    ): F[O] = seq.parApply(hl)
  }

  object sequenceNamed extends RecordArgs {
    def applyRecord[L <: HList](hl: L)(implicit seq: Sequencer[L]): seq.Out = seq(hl)
  }

  object parSequenceNamed extends RecordArgs {
    def applyRecord[F[_], L <: HList, O <: HList](hl: L)(implicit
        seq: Sequencer.Aux[F, L, O],
        par: Parallel[F]
    ): F[O] = seq.parApply(hl)
  }

  def sequenceTo[A]: SequenceOps.SequenceTo[A] =
    new SequenceOps.SequenceTo[A]
}

object SequenceOps {
  class Syntax[L <: HList](private val hl: L) extends AnyVal {
    def sequence(implicit seq: Sequencer[L]): seq.Out = seq(hl)
    def sequenceTo[A](implicit seq: GenericSequencer[A, L]): seq.F[A] = seq(hl)

    def parSequence[F[_], O <: HList](implicit
        seq: Sequencer.Aux[F, L, O],
        par: Parallel[F]
    ): F[O] = seq.parApply(hl)

    def parSequenceTo[F[_], A](implicit
        seq: GenericSequencer.Aux[F, A, L],
        par: Parallel[F]
    ): seq.F[A] = seq.parApply(hl)
  }

  class SequenceTo[A] extends ProductArgs {
    def applyProduct[L <: HList](hl: L)(implicit seq: GenericSequencer[A, L]): seq.F[A] = seq(hl)

    object par extends ProductArgs {
      def applyProduct[F[_], L <: HList](hl: L)(implicit
          seq: GenericSequencer.Aux[F, A, L],
          par: Parallel[F]
      ): seq.F[A] = seq.parApply(hl)
    }

    object named extends RecordArgs {
      def applyRecord[L <: HList](hl: L)(implicit seq: GenericSequencer[A, L]): seq.F[A] = seq(hl)
    }

    object parNamed extends RecordArgs {
      def applyRecord[F[_], L <: HList](hl: L)(implicit
          seq: GenericSequencer.Aux[F, A, L],
          par: Parallel[F]
      ): seq.F[A] = seq.parApply(hl)
    }
  }
}
