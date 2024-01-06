package cats.derived

import cats.{Apply, Semigroup, SemigroupK}
import shapeless3.deriving.{Const, K1}

import scala.annotation.*
import scala.compiletime.*
import scala.util.NotGiven

@implicitNotFound("""Could not derive an instance of SemigroupK[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T where T: Semigroup
  * it is a nested type [x] =>> G[H[x]] where G: SemigroupK
  * it is a nested type [x] =>> G[H[x]] where G: Apply and H: SemigroupK
  * it is a generic case class where all fields have a SemigroupK instance""")
type DerivedSemigroupK[F[_]] = Derived[SemigroupK[F]]
object DerivedSemigroupK:
  type Or[F[_]] = Derived.Or[SemigroupK[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: SemigroupK[F] =
    import DerivedSemigroupK.given
    summonInline[DerivedSemigroupK[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: SemigroupK[F] =
    import DerivedSemigroupK.given
    import Strict.{nested, product}
    summonInline[DerivedSemigroupK[F]].instance

  given [T](using T: Semigroup[T]): DerivedSemigroupK[Const[T]] = new SemigroupK[Const[T]]:
    def combineK[A](x: T, y: T): T = T.combine(x, y)

  given nested[F[_], G[_]](using F: => Or[F]): DerivedSemigroupK[[x] =>> F[G[x]]] =
    Strict.nested(using F.unify)

  given nested[F[_], G[_]](using ev: NotGiven[Or[F]])(using
      F: DerivedApply.Or[F],
      G: => Or[G]
  ): DerivedSemigroupK[[x] =>> F[G[x]]] =
    Strict.nested(using ev.asInstanceOf)(using F.unify, G.unify)

  given product[F[_]](using inst: => K1.ProductInstances[Or, F]): DerivedSemigroupK[F] =
    Strict.product(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedSemigroupK_F[F[_]: Or, G[_]]: DerivedSemigroupK[[x] =>> F[G[x]]] = summon

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedSemigroupK_F[F[_]: DerivedApply.Or, G[_]: Or](
      ev: NotGiven[Or[F]]
  ): DerivedSemigroupK[[x] =>> F[G[x]]] = nested(using ev)

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedSemigroupK_F[F[_]](using => K1.ProductInstances[Or, F]): DerivedSemigroupK[F] =
    summon

  trait Product[T[f[_]] <: SemigroupK[f], F[_]](using inst: K1.ProductInstances[T, F]) extends SemigroupK[F]:
    final override def combineK[A](x: F[A], y: F[A]): F[A] =
      inst.map2(x, y)([f[_]] => (F: T[f], x: f[A], y: f[A]) => F.combineK(x, y))

  object Strict:
    given nested[F[_], G[_]](using F: => SemigroupK[F]): DerivedSemigroupK[[x] =>> F[G[x]]] =
      new Derived.Lazy(() => F.compose[G]) with SemigroupK[[x] =>> F[G[x]]]:
        export delegate.*

    given nested[F[_], G[_]](using NotGiven[SemigroupK[F]])(using
        F: Apply[F],
        G: => SemigroupK[G]
    ): DerivedSemigroupK[[x] =>> F[G[x]]] = new SemigroupK[[x] =>> F[G[x]]]:
      def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = F.map2(x, y)(G.combineK)

    given product[F[_]](using => K1.ProductInstances[SemigroupK, F]): DerivedSemigroupK[F] =
      new Product[SemigroupK, F] {}
