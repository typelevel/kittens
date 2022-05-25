package cats.derived

import cats.Invariant
import shapeless3.deriving.{Const, K1}

import scala.annotation.implicitNotFound
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Invariant[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] => T
  * it is a nested type [x] => G[H[x]] where G: Invariant and H: Invariant
  * it is a generic case class where all fields have an Invariant instance
  * it is a generic sealed trait where all subclasses have an Invariant instance""")
type DerivedInvariant[F[_]] = Derived[Invariant[F]]
object DerivedInvariant:
  type Or[F[_]] = Derived.Or[Invariant[F]]
  inline def apply[F[_]]: Invariant[F] =
    import DerivedInvariant.given
    summonInline[DerivedInvariant[F]].instance

  given [T]: DerivedInvariant[Const[T]] = new Invariant[Const[T]]:
    def imap[A, B](fa: T)(f: A => B)(g: B => A): T = fa

  given [F[_]](using inst: => K1.Instances[Or, F]): DerivedInvariant[F] =
    given K1.Instances[Invariant, F] = inst.unify
    new Generic[Invariant, F] {}

  trait Generic[T[x[_]] <: Invariant[x], F[_]](using inst: K1.Instances[T, F]) extends Invariant[F]:
    def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = inst.map(fa)(
      [t[_]] => (inv: T[t], t0: t[A]) => inv.imap(t0)(f)(g)
    )
