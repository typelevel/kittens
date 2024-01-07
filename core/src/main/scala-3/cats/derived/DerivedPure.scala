package cats.derived

import alleycats.{Empty, Pure}
import shapeless3.deriving.{Const, K1}

import scala.annotation.*
import scala.compiletime.summonInline

@implicitNotFound("""Could not derive an instance of Pure[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type [x] =>> T where T: Empty
  * it is a nested type [x] =>> G[H[x]] where G: Pure and H: Pure
  * it is a generic case class where all fields have a Pure instance""")
type DerivedPure[F[_]] = Derived[Pure[F]]
object DerivedPure:
  type Or[F[_]] = Derived.Or[Pure[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Pure[F] =
    import DerivedPure.given
    summonInline[DerivedPure[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Pure[F] =
    import DerivedPure.given_DerivedPure_Const
    import Strict.given
    summonInline[DerivedPure[F]].instance

  given [T](using T: Empty[T]): DerivedPure[Const[T]] = new Pure[Const[T]]:
    def pure[A](a: A): T = T.empty

  given [T <: Singleton: ValueOf]: DerivedPure[Const[T]] = new Pure[Const[T]]:
    def pure[A](a: A): T = valueOf[T]

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedPure[[x] =>> F[G[x]]] =
    Strict.nested(using F.unify, G.unify)

  given [F[_]](using inst: K1.ProductInstances[Or, F]): DerivedPure[F] =
    Strict.product(using inst.unify)

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Or, G[_]: Or]: DerivedPure[[x] =>> F[G[x]]] = nested

  object Strict:
    given nested[F[_], G[_]](using F: => Pure[F], G: => Pure[G]): DerivedPure[[x] =>> F[G[x]]] =
      new Pure[[x] =>> F[G[x]]]:
        def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))

    given product[F[_]](using inst: K1.ProductInstances[Pure, F]): DerivedPure[F] = new Pure[F]:
      def pure[A](a: A): F[A] = inst.construct([f[_]] => (F: Pure[f]) => F.pure(a))
