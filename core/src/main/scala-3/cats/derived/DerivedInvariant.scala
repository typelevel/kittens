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

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedInvariant[[x] =>> F[G[x]]] =
    new Derived.Lazy(() => F.unify.compose(G.unify)) with Invariant[[x] =>> F[G[x]]]:
      export delegate.*

  given [F[_]](using inst: => K1.Instances[Or, F]): DerivedInvariant[F] =
    given K1.Instances[Invariant, F] = inst.unify
    new Generic[Invariant, F] {}

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedInvariant_F[F[_]: Or, G[_]: Or]: DerivedInvariant[[x] =>> F[G[x]]] = summon

  trait Generic[T[f[_]] <: Invariant[f], F[_]](using inst: K1.Instances[T, F]) extends Invariant[F]:
    final override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] =
      inst.map(fa)([f[_]] => (F: T[f], fa: f[A]) => F.imap(fa)(f)(g))
