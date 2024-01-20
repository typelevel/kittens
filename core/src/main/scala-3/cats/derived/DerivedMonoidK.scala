package cats.derived

import cats.derived.Derived.<<<
import cats.{Monoid, MonoidK}
import shapeless3.deriving.Const
import shapeless3.deriving.K1.*

import scala.annotation.*
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

  @nowarn("msg=unused import")
  inline def apply[F[_]]: MonoidK[F] =
    import DerivedMonoidK.given
    summonInline[DerivedMonoidK[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: MonoidK[F] =
    import Strict.given
    summonInline[DerivedMonoidK[F]].instance

  given [T](using T: Monoid[T]): DerivedMonoidK[Const[T]] = new MonoidK[Const[T]]:
    def empty[A]: T = T.empty
    def combineK[A](x: T, y: T): T = T.combine(x, y)

  given nested[F[_], G[_]](using F: => DerivedMonoidK.Or[F]): DerivedMonoidK[F <<< G] =
    new Derived.Lazy(() => F.unify.compose[G]) with MonoidK[F <<< G]:
      export delegate.*

  given nested[F[_], G[_]](using
      NotGiven[DerivedMonoidK.Or[F]]
  )(using F: DerivedApplicative.Or[F], G: => DerivedMonoidK.Or[G]): DerivedMonoidK[F <<< G] =
    new MonoidK[F <<< G]:
      val f = F.unify
      lazy val g = G.unify
      def empty[A]: F[G[A]] = f.pure(g.empty[A])
      def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = f.map2(x, y)(g.combineK)

  given [F[_]](using inst: => ProductInstances[Or, F]): DerivedMonoidK[F] =
    Strict.product(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using F: DerivedMonoidK.Or[F]): DerivedMonoidK[[x] =>> F[G[x]]] =
    nested(using F)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      ev: NotGiven[DerivedMonoidK.Or[F]]
  )(using F: DerivedApplicative.Or[F], G: DerivedMonoidK.Or[G]): DerivedMonoidK[[x] =>> F[G[x]]] =
    nested(using ev)

  trait Product[T[f[_]] <: MonoidK[f], F[_]](using inst: ProductInstances[T, F])
      extends MonoidK[F],
        DerivedSemigroupK.Product[T, F]:

    final override def empty[A]: F[A] =
      inst.construct([f[_]] => (F: T[f]) => F.empty[A])

  object Strict:
    given product[F[_]: ProductInstancesOf[MonoidK]]: DerivedMonoidK[F] =
      new Product[MonoidK, F] {}
