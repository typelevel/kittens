package cats.derived

import cats.{Eval, Foldable, Reducible}
import shapeless3.deriving.{Continue, K1}

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Reducible[F] where F = ${F}.
Make sure that F[_] satisfies one of the following conditions:
  * it is a nested type [x] =>> G[H[x]] where G: Reducible and H: Reducible
  * it is a generic case class where at least one field has a Reducible and the rest Foldable instances
  * it is a generic sealed trait where all subclasses have a Reducible instance""")
type DerivedReducible[F[_]] = Derived[Reducible[F]]
object DerivedReducible:
  type Or[F[_]] = Derived.Or[Reducible[F]]

  @nowarn("msg=unused import")
  inline def apply[F[_]]: Reducible[F] =
    import DerivedFoldable.given
    import DerivedReducible.given
    summonInline[DerivedReducible[F]].instance

  @nowarn("msg=unused import")
  inline def strict[F[_]]: Reducible[F] =
    import DerivedFoldable.given
    import DerivedReducible.given
    import DerivedFoldable.Strict.{nested, product}
    import DerivedReducible.Strict.{nested, product}
    summonInline[DerivedReducible[F]].instance

  given nested[F[_], G[_]](using F: => Or[F], G: => Or[G]): DerivedReducible[[x] =>> F[G[x]]] =
    Strict.nested(using F.unify, G.unify)

  def product[F[_]](ev: Reducible[?])(using inst: K1.ProductInstances[DerivedFoldable.Or, F]): DerivedReducible[F] =
    Strict.product(ev)(using inst.unify)

  inline given product[F[_]](using gen: K1.ProductGeneric[F]): DerivedReducible[F] =
    product(K1.summonFirst[Or, gen.MirroredElemTypes].unify)

  given [F[_]](using inst: => K1.CoproductInstances[Or, F]): DerivedReducible[F] =
    given K1.CoproductInstances[Reducible, F] = inst.unify
    new Coproduct[Reducible, F] {}

  @deprecated("Kept for binary compatibility", "3.2.0")
  protected given [F[_]: Or, G[_]: Or]: DerivedReducible[[x] =>> F[G[x]]] = nested

  trait Product[T[f[_]] <: Foldable[f], F[_]](@unused ev: Reducible[?])(using inst: K1.ProductInstances[T, F])
      extends DerivedFoldable.Product[T, F],
        Reducible[F]:

    private val evalNone = Eval.now(None)

    final override def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      inst
        .foldLeft[A, Option[B]](fa)(None):
          [f[_]] =>
            (acc: Option[B], F: T[f], fa: f[A]) =>
              acc match
                case Some(b) => Continue(Some(F.foldLeft(fa, b)(g)))
                case None => Continue(F.reduceLeftToOption(fa)(f)(g))
        .get

    final override def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst
        .foldRight[A, Eval[Option[B]]](fa)(evalNone):
          [f[_]] =>
            (F: T[f], fa: f[A], acc: Eval[Option[B]]) =>
              Continue(acc.flatMap {
                case Some(b) => F.foldRight(fa, Eval.now(b))(g).map(Some.apply)
                case None => F.reduceRightToOption(fa)(f)(g)
              })
        .map(_.get)

  trait Coproduct[T[f[_]] <: Reducible[f], F[_]](using inst: K1.CoproductInstances[T, F])
      extends DerivedFoldable.Coproduct[T, F],
        Reducible[F]:

    final override def reduceLeftTo[A, B](fa: F[A])(f: A => B)(g: (B, A) => B): B =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => F.reduceLeftTo(fa)(f)(g))

    final override def reduceRightTo[A, B](fa: F[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
      inst.fold(fa)([f[_]] => (F: T[f], fa: f[A]) => Eval.defer(F.reduceRightTo(fa)(f)(g)))

  object Strict:
    given nested[F[_], G[_]](using F: => Reducible[F], G: => Reducible[G]): DerivedReducible[[x] =>> F[G[x]]] =
      new Derived.Lazy(() => F.compose(G)) with Reducible[[x] =>> F[G[x]]]:
        export delegate.*

    def product[F[_]](ev: Reducible[?])(using K1.ProductInstances[Foldable, F]): DerivedReducible[F] =
      new Product[Foldable, F](ev) {}

    inline given product[F[_]](using gen: K1.ProductGeneric[F]): DerivedReducible[F] =
      product(K1.summonFirst[Reducible, gen.MirroredElemTypes])
