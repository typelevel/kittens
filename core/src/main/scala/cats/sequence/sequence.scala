/*
 * Originally adapted from shapeless-contrib scalaz
 * https://github.com/typelevel/shapeless-contrib/blob/v0.4/scalaz/main/scala/sequence.scala
 *
 */
package cats.sequence

import shapeless._
import cats.{Functor, Apply}
import shapeless.ops.hlist.ZipWithKeys
import shapeless.ops.record.{Keys, Values}

import scala.annotation.implicitNotFound

@implicitNotFound("cannot construct sequencer, make sure that every item of you hlist ${L} is an Apply")
trait Sequencer[L <: HList]  extends Serializable {
  type F[_]
  type LOut <: HList
  type Out = F[LOut]
  def apply(in: L): Out
}

trait LowPrioritySequencer {
  type Aux[L <: HList, F0[_], LOut0] = Sequencer[L] {
    type F[X] = F0[X]
    type LOut = LOut0
  }

  implicit def consSequencerAux[A[_], H, TF <: HList, T <: HList]
    (implicit st: Aux[TF, A, T], A: Apply[A]): Aux[A[H] :: TF, A, H :: T] =
      new Sequencer[A[H] :: TF] {
        type F[X] = A[X]
        type LOut = H :: T

        def apply(in: F[H] :: TF): A[LOut] = {
          A.map2(in.head, st(in.tail))(_ :: _)
        }
      }
}

object Sequencer extends LowPrioritySequencer {

  implicit def singleSequencerAux[F0[_], H](implicit F: Functor[F0])
    : Aux[F0[H] :: HNil, F0, H :: HNil] =
      new Sequencer[F0[H] :: HNil] {
        type F[X] = F0[X]
        type LOut = H :: HNil

        def apply(in: F[H] :: HNil): F[LOut] = F.map(in.head) {
          _ :: HNil
        }
      }
}


@implicitNotFound("cannot construct sequencer, make sure that every field value of you record ${L} is an Apply")
trait RecordSequencer[L <: HList] extends Serializable {
  type Out

  def apply(in: L): Out
}


object RecordSequencer extends MkRecordSequencer {
  type Aux[L <: HList, Out0] = RecordSequencer[L] { type Out = Out0 }
}

trait MkRecordSequencer {
  implicit def mkRecordSequencer[R <: HList, K <: HList, V <: HList, F[_], VOut <: HList]
  ( implicit
    V: Values.Aux[R, V],
    K: Keys.Aux[R, K],
    vSequencer: Sequencer.Aux[V, F, VOut],
    zip: ZipWithKeys[K, VOut],
    F: Functor[F]
  ): RecordSequencer.Aux[R, F[zip.Out]] =
    new RecordSequencer[R] {
      type Out = F[zip.Out]

      def apply(in: R): Out = {
        F.map(vSequencer(V(in)))(zip(_))
      }
    }
}


trait GenericSequencer[L <: HList, T] extends Serializable {
  type Out
  def apply(l: L): Out
}

object GenericSequencer extends MkGenericSequencer {
  type Aux[L <: HList, T, Out0] = GenericSequencer[L, T] { type Out = Out0 }
}

trait MkGenericSequencer {

  implicit def mkGenericSequencer[L <: HList, T, SOut <: HList, FOut, F[_]]
  ( implicit
    rs: RecordSequencer.Aux[L, FOut],
    gen: LabelledGeneric.Aux[T, SOut],
    eqv: FOut =:= F[SOut],
    F: Functor[F]
  ): GenericSequencer.Aux[L, T, F[T]] =
    new GenericSequencer[L, T] {
      type Out = F[T]

      def apply(in: L): Out = {
        F.map(rs(in))(gen.from)
      }
    }
}


trait LowPrioritySequenceOps {
  import SequenceOps._
  // fallback for non-records
  implicit def mkNonRecordOps[L <: HList](l: L): NonRecordOps[L] = new NonRecordOps(l)
}


trait SequenceOps extends LowPrioritySequenceOps {
  import SequenceOps._
  implicit def mkRecordOps[L <: HList](l: L)
                                      (implicit keys: Keys[L]): RecordOps[L] = new RecordOps(l)

  object sequence extends ProductArgs {
    def applyProduct[L <: HList](l: L)(implicit seq: Sequencer[L]): seq.Out = seq(l)
  }

  object sequenceRecord extends RecordArgs {
    def applyRecord[L <: HList](l: L)(implicit seq: RecordSequencer[L]): seq.Out = seq(l)
  }

  class sequenceGen[T] extends RecordArgs {
    def applyRecord[L <: HList](l: L)(implicit seq: GenericSequencer[L, T]): seq.Out = seq(l)
  }

  def sequenceGeneric[T] = new sequenceGen[T]

}

object SequenceOps {
  // Syntax for non-records
  class NonRecordOps[L <: HList](self: L) {
    def sequence(implicit seq: Sequencer[L]): seq.Out = seq(self)
  }

  // Syntax for records
  class RecordOps[L <: HList](self: L) {
    def sequence(implicit seq: RecordSequencer[L]): seq.Out = seq(self)
  }

}
