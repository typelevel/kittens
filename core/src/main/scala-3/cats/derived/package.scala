package cats.derived

import alleycats.*
import cats.*
import cats.kernel.{CommutativeSemigroup, CommutativeMonoid}

import scala.util.NotGiven

extension (x: Eq.type) inline def derived[A]: Eq[A] = DerivedEq[A]
extension (x: Hash.type) inline def derived[A]: Hash[A] = DerivedHash[A]
extension (x: Empty.type) inline def derived[A]: Empty[A] = DerivedEmpty[A]
extension (x: Semigroup.type) inline def derived[A]: Semigroup[A] = DerivedSemigroup[A]
extension (x: Monoid.type) inline def derived[A]: Monoid[A] = DerivedMonoid[A]
extension (x: CommutativeSemigroup.type) inline def derived[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]
extension (x: CommutativeMonoid.type) inline def derived[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]
extension (x: Foldable.type) inline def derived[F[_]]: Foldable[F] = DerivedFoldable[F]
extension (x: Functor.type) inline def derived[F[_]]: Functor[F] = DerivedFunctor[F]
extension (x: Reducible.type) inline def derived[F[_]]: Reducible[F] = DerivedReducible[F]
extension (x: Traverse.type) inline def derived[F[_]]: Traverse[F] = DerivedTraverse[F]
extension (x: NonEmptyTraverse.type) inline def derived[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse[F]

object semiauto
    extends ContravariantDerivation,
      EmptyKDerivation,
      InvariantDerivation,
      MonoidKDerivation,
      OrderDerivation,
      PartialOrderDerivation,
      SemigroupKDerivation,
      ShowDerivation,
      Instances:

  inline def eq[A]: Eq[A] = DerivedEq[A]
  inline def hash[A]: Hash[A] = DerivedHash[A]
  inline def empty[A]: Empty[A] = DerivedEmpty[A]
  inline def semigroup[A]: Semigroup[A] = DerivedSemigroup[A]
  inline def monoid[A]: Monoid[A] = DerivedMonoid[A]
  inline def commutativeSemigroup[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]
  inline def commutativeMonoid[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]
  inline def foldable[F[_]]: Foldable[F] = DerivedFoldable[F]
  inline def functor[F[_]]: Functor[F] = DerivedFunctor[F]
  inline def reducible[F[_]]: Reducible[F] = DerivedReducible[F]
  inline def traverse[F[_]]: Traverse[F] = DerivedTraverse[F]
  inline def nonEmptyTraverse[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse[F]

object auto:
  object eq:
    inline given [A](using NotGiven[Eq[A]]): Eq[A] = DerivedEq[A]

  object hash:
    inline given [A](using NotGiven[Hash[A]]): Hash[A] = DerivedHash[A]

  object empty:
    inline given [A](using NotGiven[Empty[A]]): Empty[A] = DerivedEmpty[A]

  object semigroup:
    inline given [A](using NotGiven[Semigroup[A]]): Semigroup[A] = DerivedSemigroup[A]

  object monoid:
    inline given [A](using NotGiven[Monoid[A]]): Monoid[A] = DerivedMonoid[A]

  object commutativeSemigroup:
    inline given [A](using NotGiven[CommutativeSemigroup[A]]): CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]

  object commutativeMonoid:
    inline given [A](using NotGiven[CommutativeMonoid[A]]): CommutativeMonoid[A] = DerivedCommutativeMonoid[A]

  object functor:
    inline given [F[_]](using NotGiven[Functor[F]]): Functor[F] = DerivedFunctor[F]

  object foldable:
    inline given [F[_]](using NotGiven[Foldable[F]]): Foldable[F] = DerivedFoldable[F]

  object reducible:
    inline given [F[_]](using NotGiven[Reducible[F]]): Reducible[F] = DerivedReducible[F]

  object traverse:
    inline given [F[_]](using NotGiven[Traverse[F]]): Traverse[F] = DerivedTraverse[F]

  object nonEmptyTraverse:
    inline given [F[_]](using NotGiven[NonEmptyTraverse[F]]): NonEmptyTraverse[F] = DerivedNonEmptyTraverse[F]
