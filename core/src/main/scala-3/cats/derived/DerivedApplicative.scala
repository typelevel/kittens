package cats.derived

import shapeless3.deriving.{Const, K1}
import cats.{Applicative, Monoid}

import scala.compiletime.*
import shapeless3.deriving.{Continue, K0, Labelling}

import scala.annotation.implicitNotFound
import scala.deriving.Mirror

@implicitNotFound("""Could not derive an instance of Applicative[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type λ[x => T] where T: Monoid
  * it is a nested type λ[x => G[H[x]]] where G: Applicative and H: Applicative
  * it is a generic case class where all fields have an Applicative instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector""")
type DerivedApplicative[F[_]] = Derived[Applicative[F]]
object DerivedApplicative:
  type Or[F[_]] = Derived.Or[Applicative[F]]

  inline def apply[F[_]]: Applicative[F] =
    import DerivedApplicative.given
    summonInline[DerivedApplicative[F]].instance

  given [T](using T: Monoid[T]): DerivedApplicative[Const[T]] = new Applicative[Const[T]]:
    def pure[A](x: A): Const[T][A] = T.empty
    def ap[A, B](ff: T)(fa: T): Const[T][B] = T.combine(ff, fa)

  given [F[_], G[_]](using F: Or[F], G: Or[G]): DerivedApplicative[[x] =>> F[G[x]]] =
    F.unify.compose(G.unify)

  given [F[_]](using inst: => K1.ProductInstances[Or, F]): DerivedApplicative[F] =
    given K1.ProductInstances[Applicative, F] = inst.unify
    new Product[Applicative, F] with DerivedApply.Product[Applicative, F] {}

  trait Product[T[x[_]] <: Applicative[x], F[_]](using inst: K1.ProductInstances[T, F])
      extends Applicative[F],
        DerivedApply.Product[T, F]:
    override def pure[A](x: A): F[A] = inst.construct([t[_]] => (apl: T[t]) => apl.pure[A](x))
