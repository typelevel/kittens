package cats.derived

import shapeless3.deriving.{Const, K1}
import cats.{Apply, Semigroup}

import scala.compiletime.*
import shapeless3.deriving.{Continue, K0, Labelling}

import scala.annotation.implicitNotFound
import scala.deriving.Mirror

@implicitNotFound("""Could not derive an instance of Apply[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T where T: Semigroup
  * it is a nested type [x] =>> G[H[x]] where G: Apply and H: Apply
  * it is a generic case class where all fields have an Apply instance""")
type DerivedApply[F[_]] = Derived[Apply[F]]
object DerivedApply:
  type Or[F[_]] = Derived.Or[Apply[F]]

  inline def apply[F[_]]: Apply[F] =
    import DerivedApply.given
    summonInline[DerivedApply[F]].instance

  given [T](using T: Semigroup[T]): DerivedApply[Const[T]] = new Apply[Const[T]]:
    def ap[A, B](ff: T)(fa: T) = T.combine(ff, fa)
    def map[A, B](fa: T)(f: A => B) = fa

  given [F[_], G[_]](using F: Or[F], G: Or[G]): DerivedApply[[x] =>> F[G[x]]] =
    F.unify.compose(G.unify)

  given [F[_]](using inst: => K1.ProductInstances[Or, F]): DerivedApply[F] =
    given K1.ProductInstances[Apply, F] = inst.unify
    new Product[Apply, F] {}

  trait Product[T[x[_]] <: Apply[x], F[_]](using inst: K1.ProductInstances[T, F]) extends Apply[F]:
    override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      inst.map2(ff, fa)([t[_]] => (apl: T[t], tt: t[A => B], ta: t[A]) => apl.ap(tt)(ta))
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      inst.map(fa: F[A])([f[_]] => (tf: T[f], fa: f[A]) => tf.map(fa)(f))
