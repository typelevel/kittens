package cats.derived

import cats.{Contravariant, Functor}
import shapeless3.deriving.{Const, K1}

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
    import DerivedFunctor.given_DerivedFunctor_Const
    import Strict.given
    summonInline[DerivedFunctor[F]].instance

  given [T]: DerivedFunctor[Const[T]] = new Functor[Const[T]]:
    def map[A, B](fa: T)(f: A => B): T = fa

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedFunctor[[x] =>> F[G[x]]] =
    Strict.nested(using F.unify, G.unify)

  given nested[F[_], G[_]](using
      F: DerivedContravariant.Or[F],
      G: DerivedContravariant.Or[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    Strict.nested(using F.unify, G.unify)

  given [F[_]](using inst: => K1.Instances[Or, F]): DerivedFunctor[F] =
    generic(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using F: Or[F], G: Or[G]): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      F: DerivedContravariant.Or[F],
      G: DerivedContravariant.Or[G]
  ): DerivedFunctor[[x] =>> F[G[x]]] =
    nested(using F, G)

  private def generic[F[_]](using K1.Instances[Functor, F]): DerivedFunctor[F] =
    new Generic[Functor, F] {}

  trait Generic[T[f[_]] <: Functor[f], F[_]](using inst: K1.Instances[T, F]) extends Functor[F]:
    final override def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa)([f[_]] => (F: T[f], fa: f[A]) => F.map(fa)(f))

  object Strict:
    given nested[F[_], G[_]](using F: => Functor[F], G: => Functor[G]): DerivedFunctor[[x] =>> F[G[x]]] =
      new Derived.Lazy(() => F.compose(G)) with Functor[[x] =>> F[G[x]]]:
        export delegate.*

    given nested[F[_]: Contravariant, G[_]: Contravariant]: DerivedFunctor[[x] =>> F[G[x]]] =
      Contravariant[F].compose[G]

    given product[F[_]](using K1.ProductInstances[Functor, F]): DerivedFunctor[F] =
      generic

    given coproduct[F[_]](using inst: => K1.CoproductInstances[Or, F]): DerivedFunctor[F] =
      generic(using inst.unify)
