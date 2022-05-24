package cats.derived

import cats.*
import shapeless3.deriving.{Const, K1}

import scala.annotation.implicitNotFound
import scala.compiletime.*
import scala.util.NotGiven

@implicitNotFound("""Could not derive an instance of MonoidK[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T where T: Monoid
  * it is a nested type [x] =>> G[H[x]] where G: MonoidK
  * it is a nested type [x] =>> G[H[x]] where G: Applicative and H: MonoidK
  * it is a generic case class where all fields have a MonoidK instance""")
type DerivedMonoidK[F[_]] = Derived[MonoidK[F]]
object DerivedMonoidK:
  type Or[F[_]] = Derived.Or[MonoidK[F]]
  inline def apply[F[_]]: MonoidK[F] =
    import DerivedMonoidK.given
    summonInline[DerivedMonoidK[F]].instance

  given [T](using T: Monoid[T]): DerivedMonoidK[Const[T]] = new MonoidK[Const[T]]:
    final override def empty[A]: Const[T][A] = T.empty

    final override def combineK[A](x: Const[T][A], y: Const[T][A]) = T.combine(x, y)

  given [F[_], G[_]](using F: Or[F]): DerivedMonoidK[[x] =>> F[G[x]]] =
    F.unify.compose[G]

  given [F[_], G[_]](using
      N: NotGiven[Or[F]],
      F0: DerivedApplicative.Or[F],
      G0: Or[G]
  ): DerivedMonoidK[[x] =>> F[G[x]]] =
    new MonoidK[[x] =>> F[G[x]]]:
      val F: Applicative[F] = F0.unify
      val G: MonoidK[G] = G0.unify

      final override def empty[A]: F[G[A]] = F.pure(G.empty[A])

      final override def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = F.map2(x, y)(G.combineK(_, _))

  given [F[_]](using inst: => K1.ProductInstances[Or, F]): DerivedMonoidK[F] =
    given K1.ProductInstances[MonoidK, F] = inst.unify
    new Product[MonoidK, F] {}

  trait Product[T[x[_]] <: MonoidK[x], F[_]](using inst: K1.ProductInstances[T, F])
      extends MonoidK[F],
        DerivedSemigroupK.Product[T, F]:
    final override def empty[A]: F[A] = inst.construct([t[_]] => (emp: T[t]) => emp.empty[A])
