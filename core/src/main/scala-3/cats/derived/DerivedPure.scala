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

  given [T](using T: Empty[T]): DerivedPure[Const[T]] = new Pure[Const[T]]:
    def pure[A](a: A): T = T.empty

  given [T <: Singleton: ValueOf]: DerivedPure[Const[T]] = new Pure[Const[T]]:
    def pure[A](a: A): T = valueOf[T]

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedPure[[x] =>> F[G[x]]] = new Pure[[x] =>> F[G[x]]]:
    lazy val f = F.unify
    lazy val g = G.unify
    def pure[A](a: A): F[G[A]] = f.pure(g.pure(a))

  given [F[_]](using inst: K1.ProductInstances[Or, F]): DerivedPure[F] = new Pure[F]:
    val f = inst.unify
    def pure[A](a: A): F[A] = f.construct([f[_]] => (F: Pure[f]) => F.pure(a))

  @deprecated("Kept for binary compatibility", "3.2.0")
  private[derived] def given_DerivedPure_F[F[_]: Or, G[_]: Or]: DerivedPure[[x] =>> F[G[x]]] = summon
