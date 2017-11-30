package cats.replicateH

import cats.Applicative
import cats.sequence.Sequencer
import shapeless._
import shapeless.ops.hlist._

sealed trait ReplicateH[F[_], N, A] extends Serializable {
  type LOut
  type Out = F[LOut]
  def apply(fa: F[A]): Out
}

object ReplicateH {
  type Aux[F[_], N, A, LOut0] = ReplicateH[F, N, A] { type LOut = LOut0 }

  implicit def mkReplicater[F[_], N, A, LIn <: HList, LOut <: HList](
    implicit
      filler: Fill.Aux[N, F[A], LIn],
      sequencer: Sequencer.Aux[LIn, F, LOut]
  ): Aux[F, N, A, LOut] =
    new ReplicateH[F, N, A] {
      type LOut = sequencer.LOut
      def apply(fa: F[A]): Out = sequencer(filler(fa))
    }
}

trait ReplicateHFunctions {
  def replicateH[F[_], A](n: Nat, fa: F[A])
    (implicit replicater: ReplicateH[F, n.N, A]): replicater.Out = replicater(fa)

}

trait ReplicateHOps extends {
  implicit class withReplicateH[F[_], A](self: F[A]) {
    def replicateH(n: Nat)
      (implicit replicater: ReplicateH[F, n.N, A]): replicater.Out = replicater(self)
  }
}
