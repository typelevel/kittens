package cats.derived

import cats.Invariant
import cats.derived.Derived.<<<
import shapeless3.deriving.Const
import shapeless3.deriving.K1.*

import scala.annotation.*
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

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Invariant[F] =
    import DerivedInvariant.given
    summonInline[DerivedInvariant[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Invariant[F] =
    import Strict.given
    summonInline[DerivedInvariant[F]].instance

  given [T]: DerivedInvariant[Const[T]] = new Invariant[Const[T]]:
    def imap[A, B](fa: T)(f: A => B)(g: B => A): T = fa

  given nested[F[_], G[_]](using
      F: => DerivedInvariant.Or[F],
      G: => DerivedInvariant.Or[G]
  ): DerivedInvariant[F <<< G] =
    new Derived.Lazy(() => F.unify.compose(using G.unify)) with Invariant[F <<< G]:
      export delegate.*

  given [F[_]](using inst: => Instances[Or, F]): DerivedInvariant[F] =
    generic(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: DerivedInvariant.Or, G[_]: DerivedInvariant.Or]: DerivedInvariant[[x] =>> F[G[x]]] = nested

  private def generic[F[_]: InstancesOf[Invariant]]: DerivedInvariant[F] =
    new Generic[Invariant, F] {}

  trait Generic[T[f[_]] <: Invariant[f], F[_]](using inst: Instances[T, F]) extends Invariant[F]:
    final override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] =
      inst.map(fa)([f[_]] => (F: T[f], fa: f[A]) => F.imap(fa)(f)(g))

  object Strict:
    given product[F[_]: ProductInstancesOf[Invariant]]: DerivedInvariant[F] = generic
    given coproduct[F[_]](using inst: => CoproductInstances[Or, F]): DerivedInvariant[F] =
      generic(using inst.unify)
