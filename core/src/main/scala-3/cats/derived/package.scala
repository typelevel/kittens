package cats.derived

import alleycats.*
import cats.*
import shapeless3.deriving.K0

import scala.util.NotGiven

extension (E: Empty.type) inline def derived[A]: Empty[A] = DerivedEmpty[A]

extension (S: Semigroup.type)
  inline def derived[A](using gen: K0.ProductGeneric[A]): Semigroup[A] =
    new ProductSemigroup[Semigroup, A] {}

extension (M: Monoid.type)
  inline def derived[A](using gen: K0.ProductGeneric[A]): Monoid[A] =
    new ProductMonoid[Monoid, A] {}

extension (F: Foldable.type) inline def derived[F[_]]: Foldable[F] = DerivedFoldable[F]
extension (F: Functor.type) inline def derived[F[_]]: Functor[F] = DerivedFunctor[F]
extension (F: Reducible.type) inline def derived[F[_]]: Reducible[F] = DerivedReducible[F]
extension (F: Traverse.type) inline def derived[F[_]]: Traverse[F] = DerivedTraverse[F]

object semiauto
    extends CommutativeMonoidDerivation,
      CommutativeSemigroupDerivation,
      ContravariantDerivation,
      EmptyKDerivation,
      EqDerivation,
      HashDerivation,
      InvariantDerivation,
      MonoidKDerivation,
      OrderDerivation,
      PartialOrderDerivation,
      SemigroupKDerivation,
      ShowDerivation,
      Instances:

  inline def empty[A]: Empty[A] = DerivedEmpty[A]
  inline def foldable[F[_]]: Foldable[F] = DerivedFoldable[F]
  inline def functor[F[_]]: Functor[F] = DerivedFunctor[F]
  inline def reducible[F[_]]: Reducible[F] = DerivedReducible[F]
  inline def traverse[F[_]]: Traverse[F] = DerivedTraverse[F]

object auto:
  object empty:
    inline given [A](using NotGiven[Empty[A]]): Empty[A] = DerivedEmpty[A]

  object functor:
    inline given [F[_]](using NotGiven[Functor[F]]): Functor[F] = DerivedFunctor[F]

  object foldable:
    inline given [F[_]](using NotGiven[Foldable[F]]): Foldable[F] = DerivedFoldable[F]

  object reducible:
    inline given [F[_]](using NotGiven[Reducible[F]]): Reducible[F] = DerivedReducible[F]

  object traverse:
    inline given [F[_]](using NotGiven[Traverse[F]]): Traverse[F] = DerivedTraverse[F]
