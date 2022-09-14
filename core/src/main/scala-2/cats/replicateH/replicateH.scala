package cats.replicateH

import cats.Parallel
import cats.sequence.Sequencer
import shapeless._
import shapeless.ops.hlist._

sealed trait ReplicateH[F[_], N, A] extends Serializable {
  type LOut <: HList
  type Out = F[LOut]
  def apply(fa: F[A]): Out
  def parApply(fa: F[A])(implicit par: Parallel[F]): Out
}

object ReplicateH {
  type Aux[F[_], N, A, O <: HList] = ReplicateH[F, N, A] {
    type LOut = O
  }

  implicit def fromSequencer[F[_], N, A, L <: HList, O <: HList](implicit
      fill: Fill.Aux[N, F[A], L],
      seq: Sequencer.Aux[F, L, O]
  ): Aux[F, N, A, O] = new ReplicateH[F, N, A] {
    type LOut = O
    def apply(fa: F[A]) = seq(fill(fa))
    def parApply(fa: F[A])(implicit par: Parallel[F]) = seq.parApply(fill(fa))
  }
}

trait ReplicateHOps {
  implicit def replicateHSyntax[F[_], A](fa: F[A]): ReplicateHOps.Syntax[F, A] =
    new ReplicateHOps.Syntax(fa)
}

object ReplicateHOps {
  class Syntax[F[_], A](private val self: F[A]) extends AnyVal {
    def replicateH(n: Nat)(implicit
        rep: ReplicateH[F, n.N, A]
    ): rep.Out = rep(self)

    def parReplicateH(n: Nat)(implicit
        rep: ReplicateH[F, n.N, A],
        par: Parallel[F]
    ): rep.Out = rep.parApply(self)
  }
}
