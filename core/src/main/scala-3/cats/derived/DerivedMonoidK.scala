package cats.derived

import cats.*
import shapeless3.deriving.{Const, K1}

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

  given [T](using T: Monoid[T]): DerivedMonoidK[Const[T]] = new MonoidK[Const[T]]:
    def empty[A]: T = T.empty
    def combineK[A](x: T, y: T): T = T.combine(x, y)

  given nested[F[_], G[_]](using F: => Or[F]): DerivedMonoidK[[x] =>> F[G[x]]] =
    new Derived.Lazy(() => F.unify.compose[G]) with MonoidK[[x] =>> F[G[x]]]:
      export delegate.*

  given nested[F[_], G[_]](using NotGiven[Or[F]])(using
      F: DerivedApplicative.Or[F],
      G: => Or[G]
  ): DerivedMonoidK[[x] =>> F[G[x]]] = new MonoidK[[x] =>> F[G[x]]]:
    val f: Applicative[F] = F.unify
    lazy val g: MonoidK[G] = G.unify
    def empty[A]: F[G[A]] = f.pure(g.empty[A])
    def combineK[A](x: F[G[A]], y: F[G[A]]): F[G[A]] = f.map2(x, y)(g.combineK)

  given [F[_]](using inst: => K1.ProductInstances[Or, F]): DerivedMonoidK[F] =
    given K1.ProductInstances[MonoidK, F] = inst.unify
    new Product[MonoidK, F] {}

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedMonoidK_F[F[_]: Or, G[_]]: DerivedMonoidK[[x] =>> F[G[x]]] = summon

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedMonoidK_F[F[_]: DerivedApplicative.Or, G[_]: Or](
      ev: NotGiven[Or[F]]
  ): DerivedMonoidK[[x] =>> F[G[x]]] = nested(using ev)

  trait Product[T[f[_]] <: MonoidK[f], F[_]](using inst: K1.ProductInstances[T, F])
      extends MonoidK[F],
        DerivedSemigroupK.Product[T, F]:
    final override def empty[A]: F[A] = inst.construct([f[_]] => (F: T[f]) => F.empty[A])
