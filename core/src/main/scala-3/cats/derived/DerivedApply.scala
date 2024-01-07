package cats.derived

import cats.{Apply, Semigroup}
import shapeless3.deriving.{Const, K1}

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Apply[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T where T: Semigroup
  * it is a nested type [x] =>> G[H[x]] where G: Apply and H: Apply
  * it is a generic case class where all fields have an Apply instance""")
type DerivedApply[F[_]] = Derived[Apply[F]]
object DerivedApply:
  type Or[F[_]] = Derived.Or[Apply[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Apply[F] =
    import DerivedApply.given
    summonInline[DerivedApply[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Apply[F] =
    import DerivedApply.given_DerivedApply_Const
    import Strict.given
    summonInline[DerivedApply[F]].instance

  given [T](using T: Semigroup[T]): DerivedApply[Const[T]] = new Apply[Const[T]]:
    def ap[A, B](ff: T)(fa: T): T = T.combine(ff, fa)
    def map[A, B](fa: T)(f: A => B): T = fa

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedApply[[x] =>> F[G[x]]] =
    Strict.nested(using F.unify, G.unify)

  given [F[_]](using inst: => K1.ProductInstances[Or, F]): DerivedApply[F] =
    Strict.product(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Or, G[_]: Or]: DerivedApply[[x] =>> F[G[x]]] = nested

  trait Product[T[f[_]] <: Apply[f], F[_]](using inst: K1.ProductInstances[T, F]) extends Apply[F]:
    private lazy val F = new DerivedFunctor.Generic[T, F] {}
    final override def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
    final override def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
      inst.map2(ff, fa)([f[_]] => (F: T[f], ff: f[A => B], fa: f[A]) => F.ap(ff)(fa))

  object Strict:
    given nested[F[_], G[_]](using F: => Apply[F], G: => Apply[G]): DerivedApply[[x] =>> F[G[x]]] =
      new Derived.Lazy(() => F.compose(G)) with Apply[[x] =>> F[G[x]]]:
        export delegate.*

    given product[F[_]](using K1.ProductInstances[Apply, F]): DerivedApply[F] =
      new Product[Apply, F] {}
