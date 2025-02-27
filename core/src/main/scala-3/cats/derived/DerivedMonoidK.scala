package cats.derived

import cats.{Applicative, Monoid, MonoidK}
import shapeless3.deriving.Const
import shapeless3.deriving.K1.*

import scala.annotation.*
import scala.compiletime.*
import scala.util.NotGiven

@implicitNotFound("""Could not derive MonoidK for ${F}.
Make sure it satisfies one of the following conditions:
  * constant type [x] =>> T where T: Monoid
  * nested type [x] =>> G[H[x]] where G: MonoidK
  * nested type [x] =>> G[H[x]] where G: Applicative and H: MonoidK
  * generic case class where all fields form MonoidK""")
type DerivedMonoidK[F[_]] = Derived[MonoidK[F]]
object DerivedMonoidK:
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

  given nested[F[_], G[_]](using F: => Derived.Or[MonoidK[F]]): DerivedMonoidK[F <<< G] =
    new Derived.Lazy(() => F.compose[G]) with MonoidK[F <<< G]:
      export delegate.*

  given nested[F[_], G[_]](using
      NotGiven[Derived.Or[MonoidK[F]]]
  )(using F: Derived.Or[Applicative[F]], G: => Derived.Or[MonoidK[G]]): DerivedMonoidK[F <<< G] =
    new MonoidK[F <<< G]:
      lazy val g = G
      def empty[A]: F[G[A]] = F.pure(g.empty[A])
      def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = F.map2(x, y)(g.combineK)

  given [F[_]](using inst: => ProductInstances[Derived.Or1[MonoidK], F]): DerivedMonoidK[F] =
    Strict.product

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using F: Derived.Or[MonoidK[F]]): DerivedMonoidK[[x] =>> F[G[x]]] =
    nested(using F)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_], G[_]](using
      ev: NotGiven[Derived.Or[MonoidK[F]]]
  )(using F: Derived.Or[Applicative[F]], G: Derived.Or[MonoidK[G]]): DerivedMonoidK[[x] =>> F[G[x]]] =
    nested(using ev)

  trait Product[T[f[_]] <: MonoidK[f], F[_]](using inst: ProductInstances[T, F])
      extends MonoidK[F],
        DerivedSemigroupK.Product[T, F]:

    final override def empty[A]: F[A] =
      inst.construct([f[_]] => (F: T[f]) => F.empty[A])

  object Strict:
    given product[F[_]: ProductInstancesOf[MonoidK]]: DerivedMonoidK[F] =
      new Product[MonoidK, F] {}
