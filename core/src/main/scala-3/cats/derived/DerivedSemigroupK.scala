package cats.derived

import cats.{Semigroup, SemigroupK}
import shapeless3.deriving.{Const, K1}

import scala.compiletime.*

type DerivedSemigroupK[F[_]] = Derived[SemigroupK[F]]
object DerivedSemigroupK extends DerivedSemigroupKInstances1:
  type Or[F[_]] = Derived.Or[SemigroupK[F]]
  inline def apply[F[_]]: SemigroupK[F] =
    import DerivedSemigroupK.given
    summonInline[DerivedSemigroupK[F]].instance

  given [T](using T: Semigroup[T]): DerivedSemigroupK[Const[T]] = new SemigroupK[Const[T]]:
    final override def combineK[A](x: Const[T][A], y: Const[T][A]) = T.combine(x, y)

  given [F[_], G[_]](using F: Or[F]): DerivedSemigroupK[[x] =>> F[G[x]]] =
    F.unify.compose[G]

  given [F[_]](using inst: => K1.ProductInstances[Or, F]): DerivedSemigroupK[F] =
    given K1.ProductInstances[SemigroupK, F] = inst.unify
    new Product[SemigroupK, F] {}

  trait Product[T[x[_]] <: SemigroupK[x], F[_]](using inst: K1.ProductInstances[T, F]) extends SemigroupK[F]:
    final override def combineK[A](x: F[A], y: F[A]): F[A] =
      inst.map2[A, A, A](x, y)([t[_]] => (smgrpk: T[t], x: t[A], y: t[A]) => smgrpk.combineK(x, y))

trait DerivedSemigroupKInstances1:
  import DerivedSemigroupK.Or

  given [F[_], G[_]](using F: DerivedApply.Or[F], G: Or[G]): DerivedSemigroupK[[x] =>> F[G[x]]] =
    new SemigroupK[[x] =>> F[G[x]]]:
      final override def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = F.unify.map2(x, y)(G.unify.combineK(_, _))
