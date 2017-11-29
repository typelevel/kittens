package cats.replicateH

import cats.sequence.Sequencer
import shapeless._
import shapeless.ops.hlist._

sealed trait ReplicateH[F[_], N, A] extends Serializable {
  type Out
  def apply(fa: F[A]): Out
}

object ReplicateH {
  type Aux[F[_], N, A, Out0] = ReplicateH[F, N, A] { type Out = Out0 }

  implicit def mkReplicater[F[_], N, A, L <: HList](
    implicit
      filler: Fill.Aux[N, F[A], L],
      sequencer: Sequencer[L]
  ): Aux[F, N, A, sequencer.Out] =
    new ReplicateH[F, N, A] {
      type Out = sequencer.Out
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
