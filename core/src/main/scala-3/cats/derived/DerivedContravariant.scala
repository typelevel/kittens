package cats.derived

import cats.{Contravariant, Functor}
import shapeless3.deriving.{Const, K1}

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Contravariant[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T
  * it is a nested type [x] =>> G[H[x]] where G: Functor and H: Contravariant
  * it is a generic case class where all fields have a Contravariant instance
  * it is a generic sealed trait where all subclasses have a Contravariant instance""")
type DerivedContravariant[F[_]] = Derived[Contravariant[F]]
object DerivedContravariant:
  type Or[F[_]] = Derived.Or[Contravariant[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Contravariant[F] =
    import DerivedContravariant.given
    summonInline[DerivedContravariant[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Contravariant[F] =
    import DerivedContravariant.given_DerivedContravariant_Const
    import Strict.given
    summonInline[DerivedContravariant[F]].instance

  given [T]: DerivedContravariant[Const[T]] = new Contravariant[Const[T]]:
    def contramap[A, B](fa: T)(f: B => A): T = fa

  given nested[F[_], G[_]](using F: DerivedFunctor.Or[F], G: => Or[G]): DerivedContravariant[[x] =>> F[G[x]]] =
    Strict.nested(using F.unify, G.unify)

  given [F[_]](using inst: => K1.Instances[Or, F]): DerivedContravariant[F] =
    generic(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: DerivedFunctor.Or, G[_]: Or]: DerivedContravariant[[x] =>> F[G[x]]] = nested

  private def generic[F[_]](using K1.Instances[Contravariant, F]): DerivedContravariant[F] =
    new Generic[Contravariant, F] {}

  trait Generic[T[f[_]] <: Contravariant[f], F[_]](using inst: K1.Instances[T, F]) extends Contravariant[F]:
    final override def contramap[A, B](fa: F[A])(f: B => A): F[B] =
      inst.map(fa)([f[_]] => (T: T[f], fa: f[A]) => T.contramap(fa)(f))

  object Strict:
    given nested[F[_], G[_]](using F: Functor[F], G: => Contravariant[G]): DerivedContravariant[[x] =>> F[G[x]]] =
      new Derived.Lazy(() => F.composeContravariant(G)) with Contravariant[[x] =>> F[G[x]]]:
        export delegate.*

    given product[F[_]](using K1.ProductInstances[Contravariant, F]): DerivedContravariant[F] =
      generic

    given coproduct[F[_]](using inst: => K1.CoproductInstances[Or, F]): DerivedContravariant[F] =
      generic(using inst.unify)
