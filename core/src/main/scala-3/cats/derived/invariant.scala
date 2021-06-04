package cats.derived

import cats.Invariant
import shapeless3.deriving.K1

trait GenericInvariant[T[x[_]] <: Invariant[x], F[_]](using inst: K1.Instances[T, F])
  extends Invariant[F]:
    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = inst.map(fa)(
      [t[_]] => (inv: T[t], t0: t[A]) => inv.imap(t0)(f)(g)
    )

trait InvariantDerivation:
  extension (F: Invariant.type)
    inline def derived[F[_]](using gen: K1.Generic[F]): Invariant[F] =
      new GenericInvariant[Invariant, F]{}
