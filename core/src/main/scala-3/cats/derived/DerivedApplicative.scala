package cats.derived

import cats.{Applicative, Monoid}
import shapeless3.deriving.{Const, K1}

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Applicative[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T where T: Monoid
  * it is a nested type [x] =>> G[H[x]] where G: Applicative and H: Applicative
  * it is a generic case class where all fields have an Applicative instance""")
type DerivedApplicative[F[_]] = Derived[Applicative[F]]
object DerivedApplicative:
  type Or[F[_]] = Derived.Or[Applicative[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Applicative[F] =
    import DerivedApplicative.given
    summonInline[DerivedApplicative[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Applicative[F] =
    import Strict.given
    summonInline[DerivedApplicative[F]].instance

  given [T](using T: Monoid[T]): DerivedApplicative[Const[T]] = new Applicative[Const[T]]:
    def pure[A](x: A): T = T.empty
    def ap[A, B](ff: T)(fa: T): T = T.combine(ff, fa)

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedApplicative[[x] =>> F[G[x]]] =
    new Derived.Lazy(() => F.unify.compose(using G.unify)) with Applicative[[x] =>> F[G[x]]]:
      export delegate.*

  given [F[_]](using inst: => K1.ProductInstances[Or, F]): DerivedApplicative[F] =
    Strict.product(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Or, G[_]: Or]: DerivedApplicative[[x] =>> F[G[x]]] = nested

  trait Product[T[f[_]] <: Applicative[f], F[_]](using inst: K1.ProductInstances[T, F])
      extends Applicative[F],
        DerivedApply.Product[T, F]:

    final override def pure[A](x: A): F[A] =
      inst.construct([f[_]] => (F: T[f]) => F.pure[A](x))

  object Strict:
    given product[F[_]](using K1.ProductInstances[Applicative, F]): DerivedApplicative[F] =
      new Product[Applicative, F] with DerivedApply.Product[Applicative, F] {}
