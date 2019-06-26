/*
 * Originally adapted from shapeless-contrib scalaz
 * https://github.com/typelevel/shapeless-contrib/blob/v0.4/scalaz/main/scala/sequence.scala
 *
 */
package cats.sequence

import cats.{Applicative, Apply, Functor}
import shapeless._
import shapeless.ops.hlist.{Align, ZipWithKeys}
import shapeless.ops.record.{Keys, UnzipFields}

import scala.annotation.implicitNotFound

@implicitNotFound("cannot construct sequencer, make sure that every item of your hlist ${L} is an Apply")
trait Sequencer[L <: HList] extends Serializable {
  type F[_]
  type LOut <: HList
  type Out = F[LOut]
  def apply(hl: L): Out
}

private[sequence] abstract class MkHConsSequencer {

  type Aux[L <: HList, F0[_], LOut0] = Sequencer[L] {
    type F[X] = F0[X]
    type LOut = LOut0
  }

  implicit def mkHConsSequencer[F0[_], H, FT <: HList, T <: HList](
    implicit tailSequencer: Aux[FT, F0, T], F: Apply[F0]
  ): Aux[F0[H] :: FT, F0, H :: T] = new Sequencer[F0[H] :: FT] {
    type F[X] = F0[X]
    type LOut = H :: T
    def apply(hl: F[H] :: FT) = F.map2(hl.head, tailSequencer(hl.tail))(_ :: _)
  }
}

object Sequencer extends MkHConsSequencer {

  implicit def mkHNilSequencer[F0[_]](
    implicit F: Applicative[F0]
  ): Aux[HNil, F0, HNil] = new Sequencer[HNil] {
    type F[X] = F0[X]
    type LOut = HNil
    def apply(nil: HNil) = F.pure(nil)
  }

  implicit def mkSingletonSequencer[F0[_], H](
    implicit F: Functor[F0]
  ): Aux[F0[H] :: HNil, F0, H :: HNil] = new Sequencer[F0[H] :: HNil] {
    type F[X] = F0[X]
    type LOut = H :: HNil
    def apply(singleton: F[H] :: HNil) = F.map(singleton.head)(_ :: HNil)
  }
}

@implicitNotFound("cannot construct sequencer, make sure that every field value of you record ${L} is an Apply")
trait RecordSequencer[L <: HList] extends Serializable {
  type Out
  def apply(record: L): Out
}

object RecordSequencer {
  type Aux[L <: HList, Out0] = RecordSequencer[L] { type Out = Out0 }

  implicit def mkRecordSequencer[R <: HList, K <: HList, V <: HList, F[_], VOut <: HList](
    implicit unzip: UnzipFields.Aux[R, K, V],
    valueSequencer: Sequencer.Aux[V, F, VOut],
    F: Functor[F],
    zip: ZipWithKeys[K, VOut]
  ): RecordSequencer.Aux[R, F[zip.Out]] = new RecordSequencer[R] {
    type Out = F[zip.Out]
    def apply(record: R) = F.map(valueSequencer(unzip.values(record)))(zip(_))
  }
}

trait GenericSequencer[L <: HList, T] extends Serializable {
  type Out
  def apply(hl: L): Out
}

object GenericSequencer {
  type Aux[L <: HList, T, Out0] = GenericSequencer[L, T] { type Out = Out0 }

  implicit def mkGenericSequencer[L <: HList, T, SOut <: HList, FOut, LOut <: HList, F[_]](
    implicit recordSequencer: RecordSequencer.Aux[L, FOut],
    eqv: FOut =:= F[SOut],
    F: Functor[F],
    gen: LabelledGeneric.Aux[T, LOut],
    align: Align[SOut, LOut]
  ): GenericSequencer.Aux[L, T, F[T]] = new GenericSequencer[L, T] {
    type Out = F[T]
    def apply(hl: L) = F.map(recordSequencer(hl))(so => gen.from(so.align))
  }
}

private[sequence] trait MkNonRecordOps {
  import SequenceOps._

  // fallback for non-records
  implicit def mkNonRecordOps[L <: HList](hl: L): NonRecordOps[L] =
    new NonRecordOps(hl)
}

trait SequenceOps extends MkNonRecordOps {
  import SequenceOps._

  implicit def mkRecordOps[L <: HList: Keys](hl: L): RecordOps[L] =
    new RecordOps(hl)

  object sequence extends ProductArgs {
    def applyProduct[L <: HList](hl: L)(implicit seq: Sequencer[L]): seq.Out = seq(hl)
  }

  object sequenceRecord extends RecordArgs {
    def applyRecord[L <: HList](hl: L)(implicit seq: RecordSequencer[L]): seq.Out = seq(hl)
  }

  class sequenceGen[T] extends RecordArgs {
    def applyRecord[L <: HList](hl: L)(implicit seq: GenericSequencer[L, T]): seq.Out = seq(hl)
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
