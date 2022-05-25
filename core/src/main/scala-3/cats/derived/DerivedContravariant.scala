package cats.derived

import cats.{Contravariant, Functor}
import shapeless3.deriving.{Const, K1}

import scala.annotation.implicitNotFound
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
  inline def apply[F[_]]: Contravariant[F] =
    import DerivedContravariant.given
    summonInline[DerivedContravariant[F]].instance

  given [T]: DerivedContravariant[Const[T]] = new Contravariant[Const[T]]:
    def contramap[A, B](fa: T)(f: B => A): T = fa

  given [F[_], G[_]](using F: DerivedFunctor.Or[F], G: Or[G]): DerivedContravariant[[x] =>> F[G[x]]] =
    given Contravariant[G] = G.unify
    F.unify.composeContravariant[G]

  given [F[_]](using inst: => K1.Instances[Or, F]): DerivedContravariant[F] =
    given K1.Instances[Contravariant, F] = inst.unify
    new Generic[Contravariant, F] {}

  trait Generic[T[x[_]] <: Contravariant[x], F[_]](using inst: K1.Instances[T, F]) extends Contravariant[F]:
    final override def contramap[A, B](fa: F[A])(f: B => A): F[B] =
      inst.map(fa: F[A])([f[_]] => (tf: T[f], fa: f[A]) => tf.contramap(fa)(f))
