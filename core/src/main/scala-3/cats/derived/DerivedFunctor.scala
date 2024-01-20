package cats.derived

import cats.Functor
import cats.derived.Derived.<<<
import shapeless3.deriving.Const
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Functor[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T
  * it is a nested type [x] =>> G[H[x]] where G: Functor and H: Functor
  * it is a nested type [x] =>> G[H[x]] where G: Contravariant and H: Contravariant
  * it is a generic case class where all fields have a Functor instance
  * it is a generic sealed trait where all subclasses have a Functor instance""")
type DerivedFunctor[F[_]] = Derived[Functor[F]]
object DerivedFunctor:
  type Or[F[_]] = Derived.Or[Functor[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Functor[F] =
    import DerivedFunctor.given
    summonInline[DerivedFunctor[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Functor[F] =
    import Strict.given
    summonInline[DerivedFunctor[F]].instance

  given [T]: DerivedFunctor[Const[T]] = new Functor[Const[T]]:
    def map[A, B](fa: T)(f: A => B): T = fa

  given nested[F[_], G[_]](using F: => DerivedFunctor.Or[F], G: => DerivedFunctor.Or[G]): DerivedFunctor[F <<< G] =
    new Derived.Lazy(() => F.unify.compose(using G.unify)) with Functor[F <<< G]:
      export delegate.*

  given nested[F[_], G[_]](using
      F: DerivedContravariant.Or[F],
      G: DerivedContravariant.Or[G]
  ): DerivedFunctor[F <<< G] =
    F.unify.compose(using G.unify)

  given [F[_]](using inst: => Instances[Or, F]): DerivedFunctor[F] =
    generic(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: DerivedFunctor.Or[F],
      G: DerivedFunctor.Or[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: DerivedContravariant.Or[F],
      G: DerivedContravariant.Or[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  private def generic[F[_]: InstancesOf[Functor]]: DerivedFunctor[F] =
    new Generic[Functor, F] {}

  trait Generic[T[f[_]] <: Functor[f], F[_]](using inst: Instances[T, F]) extends Functor[F]:
    final override def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa)([f[_]] => (F: T[f], fa: f[A]) => F.map(fa)(f))

  object Strict:
    given product[F[_]: ProductInstancesOf[Functor]]: DerivedFunctor[F] = generic
    given coproduct[F[_]](using inst: => CoproductInstances[Or, F]): DerivedFunctor[F] =
      generic(using inst.unify)
