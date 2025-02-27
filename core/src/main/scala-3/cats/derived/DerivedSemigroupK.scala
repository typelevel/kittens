package cats.derived

import cats.{Apply, Semigroup, SemigroupK}
import shapeless3.deriving.Const
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
  @nowarn("msg=unused import")
  inline def apply[F[_]]: SemigroupK[F] =
    import DerivedSemigroupK.given
    summonInline[DerivedSemigroupK[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: SemigroupK[F] =
    import Strict.given
    summonInline[DerivedSemigroupK[F]].instance

  given [T](using T: Semigroup[T]): DerivedSemigroupK[Const[T]] = new SemigroupK[Const[T]]:
    def combineK[A](x: T, y: T): T = T.combine(x, y)

  given nested[F[_], G[_]](using F: => Derived.Or[SemigroupK[F]]): DerivedSemigroupK[F <<< G] =
    new Derived.Lazy(() => F.compose[G]) with SemigroupK[F <<< G]:
      export delegate.*

  given nested[F[_], G[_]](using
      NotGiven[Derived.Or[SemigroupK[F]]]
  )(using F: Derived.Or[Apply[F]], G: => Derived.Or[SemigroupK[G]]): DerivedSemigroupK[F <<< G] =
    new SemigroupK[F <<< G]:
      lazy val g = G
      def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = F.map2(x, y)(g.combineK)

  given [F[_]](using inst: => ProductInstances[Derived.Or1[SemigroupK], F]): DerivedSemigroupK[F] =
    Strict.product

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using F: Derived.Or[SemigroupK[F]]): DerivedSemigroupK[[x] =>> F[G[x]]] =
    nested(using F)

  @deprecated("Kept for binary compati`bility", "3.2.0")
  protected given [F[_], G[_]](using
      ev: NotGiven[Derived.Or[SemigroupK[F]]]
  )(using F: Derived.Or[Apply[F]], G: Derived.Or[SemigroupK[G]]): DerivedSemigroupK[[x] =>> F[G[x]]] =
    nested(using ev)

  trait Product[T[f[_]] <: SemigroupK[f], F[_]](using inst: ProductInstances[T, F]) extends SemigroupK[F]:
    final override def combineK[A](x: F[A], y: F[A]): F[A] =
      inst.map2(x, y)([f[_]] => (F: T[f], x: f[A], y: f[A]) => F.combineK(x, y))

  object Strict:
    given product[F[_]: ProductInstancesOf[SemigroupK]]: DerivedSemigroupK[F] =
      new Product[SemigroupK, F] {}
