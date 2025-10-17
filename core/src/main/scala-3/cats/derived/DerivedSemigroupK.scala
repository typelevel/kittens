package cats.derived

import cats.{Apply, Semigroup, SemigroupK}
import shapeless3.deriving.{Const, Derived}
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*
import scala.util.NotGiven

@implicitNotFound("""Could not derive SemigroupK for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T where T: Semigroup
  * nested type [x] =>> G[H[x]] where G: SemigroupK
  * nested type [x] =>> G[H[x]] where G: Apply and H: SemigroupK
  * generic case class where all fields form SemigroupK""")
type DerivedSemigroupK[F[_]] = Derived[SemigroupK[F]]
object DerivedSemigroupK:
  inline def apply[F[_]]: SemigroupK[F] =
    import DerivedSemigroupK.given
    summonInline[DerivedSemigroupK[F]].instance

  inline def strict[F[_]]: SemigroupK[F] =
    import Strict.given
    summonInline[DerivedSemigroupK[F]].instance

  given [T](using T: Semigroup[T]): DerivedSemigroupK[Const[T]] = new SemigroupK[Const[T]]:
    def combineK[A](x: T, y: T): T = T.combine(x, y)

  given nested[F[_], G[_]](using F: => (SemigroupK |: Derived)[F]): DerivedSemigroupK[F <<< G] =
    new Lazy(() => F.unify.compose[G]) with SemigroupK[F <<< G]:
      export delegate.*

  given nested[F[_], G[_]](using
      NotGiven[(SemigroupK |: Derived)[F]]
  )(using F: (Apply |: Derived)[F], G: => (SemigroupK |: Derived)[G]): DerivedSemigroupK[F <<< G] =
    new SemigroupK[F <<< G]:
      val f = F.unify
      lazy val g = G.unify
      def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = f.map2(x, y)(g.combineK)

  given [F[_]](using inst: => ProductInstances[SemigroupK |: Derived, F]): DerivedSemigroupK[F] =
    Strict.product(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using F: (SemigroupK |: Derived)[F]): DerivedSemigroupK[[x] =>> F[G[x]]] =
    nested(using F)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      ev: NotGiven[(SemigroupK |: Derived)[F]]
  )(using F: (Apply |: Derived)[F], G: (SemigroupK |: Derived)[G]): DerivedSemigroupK[[x] =>> F[G[x]]] =
    nested(using ev)

  trait Product[T[f[_]] <: SemigroupK[f], F[_]](using inst: ProductInstances[T, F]) extends SemigroupK[F]:
    final override def combineK[A](x: F[A], y: F[A]): F[A] =
      inst.map2(x, y)([f[_]] => (F: T[f], x: f[A], y: f[A]) => F.combineK(x, y))

  object Strict:
    given product[F[_]: ProductInstancesOf[SemigroupK]]: DerivedSemigroupK[F] =
      new Product[SemigroupK, F] {}
