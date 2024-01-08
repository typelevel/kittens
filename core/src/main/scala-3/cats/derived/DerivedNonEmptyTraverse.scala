package cats.derived

import cats.{Applicative, Apply, NonEmptyTraverse, Traverse}
import shapeless3.deriving.K1

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of NonEmptyTraverse[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a nested type [x] =>> G[H[x]] where G: NonEmptyTraverse and H: NonEmptyTraverse
  * it is a generic case class where at least one field has a NonEmptyTraverse and the rest Traverse instances
  * it is a generic sealed trait where all subclasses have a NonEmptyTraverse instance""")
type DerivedNonEmptyTraverse[F[_]] = Derived[NonEmptyTraverse[F]]
object DerivedNonEmptyTraverse:
  type Or[F[_]] = Derived.Or[NonEmptyTraverse[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: NonEmptyTraverse[F] =
    import DerivedTraverse.given
    import DerivedNonEmptyTraverse.given
    summonInline[DerivedNonEmptyTraverse[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: NonEmptyTraverse[F] =
    import DerivedTraverse.Strict.given
    import DerivedNonEmptyTraverse.Strict.given
    summonInline[DerivedNonEmptyTraverse[F]].instance

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedNonEmptyTraverse[[x] =>> F[G[x]]] =
    new Derived.Lazy(() => F.unify.compose(using G.unify)) with NonEmptyTraverse[[x] =>> F[G[x]]]:
      export delegate.*

  def product[F[_]](ev: NonEmptyTraverse[?])(using
      inst: K1.ProductInstances[DerivedTraverse.Or, F]
  ): DerivedNonEmptyTraverse[F] =
    Strict.product(ev)(using inst.unify)

  inline given product[F[_]](using gen: K1.ProductGeneric[F]): DerivedNonEmptyTraverse[F] =
    product(K1.summonFirst[Or, gen.MirroredElemTypes].unify)

  given [F[_]](using => K1.CoproductInstances[Or, F]): DerivedNonEmptyTraverse[F] =
    Strict.coproduct

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Or, G[_]: Or]: DerivedNonEmptyTraverse[[x] =>> F[G[x]]] = nested

  trait Product[T[x[_]] <: Traverse[x], F[_]](@unused ev: NonEmptyTraverse[?])(using
      @unused inst: K1.ProductInstances[T, F]
  ) extends NonEmptyTraverse[F],
        DerivedReducible.Product[T, F],
        DerivedTraverse.Product[T, F]:

    final override def nonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      traverse[Alt[G], A, B](fa)(f.andThen(Left.apply)) match
        case Left(value) => value
        case Right(_) => ???

  trait Coproduct[T[x[_]] <: NonEmptyTraverse[x], F[_]](using
      inst: K1.CoproductInstances[T, F]
  ) extends NonEmptyTraverse[F],
        DerivedReducible.Coproduct[T, F],
        DerivedTraverse.Coproduct[T, F]:

    final override def nonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => F.nonEmptyTraverse(fa)(f).asInstanceOf[G[F[B]]])

  private type Alt[F[_]] = [A] =>> Either[F[A], A]
  private given [F[_]](using F: Apply[F]): Applicative[Alt[F]] with
    override def pure[A](x: A) = Right(x)
    override def map[A, B](fa: Alt[F][A])(f: A => B) = fa match
      case Left(fa) => Left(F.map(fa)(f))
      case Right(a) => Right(f(a))
    override def ap[A, B](ff: Alt[F][A => B])(fa: Alt[F][A]) = (ff, fa) match
      case (Left(ff), Left(fa)) => Left(F.ap(ff)(fa))
      case (Left(ff), Right(a)) => Left(F.map(ff)(_(a)))
      case (Right(f), Left(fa)) => Left(F.map(fa)(f))
      case (Right(f), Right(a)) => Right(f(a))

  object Strict:
    def product[F[_]](ev: NonEmptyTraverse[?])(using K1.ProductInstances[Traverse, F]): DerivedNonEmptyTraverse[F] =
      new Product[Traverse, F](ev)
        with DerivedReducible.Product[Traverse, F](ev)
        with DerivedTraverse.Product[Traverse, F]
        with DerivedFunctor.Generic[Traverse, F] {}

    inline given product[F[_]](using gen: K1.ProductGeneric[F]): DerivedNonEmptyTraverse[F] =
      product(K1.summonFirst[NonEmptyTraverse, gen.MirroredElemTypes])

    given coproduct[F[_]](using inst: => K1.CoproductInstances[Or, F]): DerivedNonEmptyTraverse[F] =
      given K1.CoproductInstances[NonEmptyTraverse, F] = inst.unify
      new NonEmptyTraverse[F] with Coproduct[NonEmptyTraverse, F] {}
