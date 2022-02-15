package cats.derived

import alleycats.{Empty, EmptyK}
import shapeless3.deriving.{Const, K1}
import scala.compiletime.summonInline
import scala.annotation.implicitNotFound
import alleycats.Pure
import scala.util.NotGiven
import cats.data.NonEmptyListInstances0

@implicitNotFound("""Could not derive an instance of EmptyK[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a constant type λ[x => T] where T: Empty
  * it is a nested type λ[x => G[H[x]]] where G: EmptyK
  * it is a nested type λ[x => G[H[x]]] where G: Pure and H: EmptyK
  * it is a generic case class where all fields have an EmptyK instance

Note: using kind-projector notation - https://github.com/typelevel/kind-projector""")
type DerivedEmptyK[F[_]] = Derived[EmptyK[F]]
object DerivedEmptyK:
  type Or[F[_]] = Derived.Or[EmptyK[F]]
  inline def apply[F[_]]: EmptyK[F] =
    import DerivedEmptyK.given
    summonInline[DerivedEmptyK[F]].instance

  given [T](using T: Empty[T]): DerivedEmptyK[Const[T]] = new EmptyK[Const[T]]:
    def empty[A] = T.empty

  given or[F[_], G[_]](using F: Or[F]): DerivedEmptyK[[x] =>> F[G[x]]] = new EmptyK[[x] =>> F[G[x]]]:
    def empty[A] = F.unify.empty

  given pureor[F[_], G[_]](using NotGiven[Or[F]])(using F: Pure[F], G: Or[G]): DerivedEmptyK[[x] =>> F[G[x]]] = new EmptyK[[x] =>> F[G[x]]]:
    def empty[A] = F.pure(G.unify.empty)

  given [F[_]](using inst: K1.ProductInstances[Or, F]): DerivedEmptyK[F] =
    given K1.ProductInstances[EmptyK, F] = inst.unify
    new Product[EmptyK, F] {}

  trait Product[T[x[_]] <: EmptyK[x], F[_]](using
      inst: K1.ProductInstances[T, F]
  ) extends EmptyK[F]:
    def empty[A]: F[A] = inst.construct([t[_]] => (emp: T[t]) => emp.empty[A])
