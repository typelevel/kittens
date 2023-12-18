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
extension (x: Order.type) inline def derived[A]: Order[A] = DerivedOrder[A]
extension (x: CommutativeSemigroup.type) inline def derived[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]
extension (x: CommutativeMonoid.type) inline def derived[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]
extension (x: Show.type) inline def derived[A]: Show[A] = DerivedShow[A]
extension (x: Applicative.type) inline def derived[F[_]]: Applicative[F] = DerivedApplicative[F]
extension (x: Apply.type) inline def derived[F[_]]: Apply[F] = DerivedApply[F]
extension (x: EmptyK.type) inline def derived[F[_]]: EmptyK[F] = DerivedEmptyK[F]
extension (x: Pure.type) inline def derived[F[_]]: Pure[F] = DerivedPure[F]
extension (x: Foldable.type) inline def derived[F[_]]: Foldable[F] = DerivedFoldable[F]
extension (x: Functor.type) inline def derived[F[_]]: Functor[F] = DerivedFunctor[F]
extension (x: Reducible.type) inline def derived[F[_]]: Reducible[F] = DerivedReducible[F]
extension (x: Traverse.type) inline def derived[F[_]]: Traverse[F] = DerivedTraverse[F]
extension (x: NonEmptyTraverse.type) inline def derived[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse[F]
extension (x: SemigroupK.type) inline def derived[F[_]]: SemigroupK[F] = DerivedSemigroupK[F]
extension (x: MonoidK.type) inline def derived[F[_]]: MonoidK[F] = DerivedMonoidK[F]
extension (x: Contravariant.type) inline def derived[F[_]]: Contravariant[F] = DerivedContravariant[F]
extension (x: Invariant.type) inline def derived[F[_]]: Invariant[F] = DerivedInvariant[F]
extension (x: PartialOrder.type) inline def derived[A]: PartialOrder[A] = DerivedPartialOrder[A]
extension (x: ShowPretty.type) inline def derived[A]: ShowPretty[A] = DerivedShowPretty[A]

object semiauto:
  inline def eq[A]: Eq[A] = DerivedEq[A]
  inline def hash[A]: Hash[A] = DerivedHash[A]
  inline def empty[A]: Empty[A] = DerivedEmpty[A]
  inline def semigroup[A]: Semigroup[A] = DerivedSemigroup[A]
  inline def monoid[A]: Monoid[A] = DerivedMonoid[A]
  inline def order[A]: Order[A] = DerivedOrder[A]
  inline def commutativeSemigroup[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]
  inline def commutativeMonoid[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]
  inline def applicative[F[_]]: Applicative[F] = DerivedApplicative[F]
  inline def apply[F[_]]: Apply[F] = DerivedApply[F]
  inline def emptyK[F[_]]: EmptyK[F] = DerivedEmptyK[F]
  inline def pure[F[_]]: Pure[F] = DerivedPure[F]
  inline def foldable[F[_]]: Foldable[F] = DerivedFoldable[F]
  inline def functor[F[_]]: Functor[F] = DerivedFunctor[F]
  inline def reducible[F[_]]: Reducible[F] = DerivedReducible[F]
  inline def traverse[F[_]]: Traverse[F] = DerivedTraverse[F]
  inline def nonEmptyTraverse[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse[F]
  inline def show[A]: Show[A] = DerivedShow[A]
  inline def semigroupK[F[_]]: SemigroupK[F] = DerivedSemigroupK[F]
  inline def monoidK[F[_]]: MonoidK[F] = DerivedMonoidK[F]
  inline def contravariant[F[_]]: Contravariant[F] = DerivedContravariant[F]
  inline def invariant[F[_]]: Invariant[F] = DerivedInvariant[F]
  inline def partialOrder[A]: PartialOrder[A] = DerivedPartialOrder[A]
  inline def showPretty[A]: ShowPretty[A] = DerivedShowPretty[A]

object strict:
  extension (x: Eq.type) inline def derived[A]: Eq[A] = DerivedEq.strict[A]
  extension (x: Hash.type) inline def derived[A]: Hash[A] = DerivedHash.strict[A]
  extension (x: Empty.type) inline def derived[A]: Empty[A] = DerivedEmpty.strict[A]
  extension (x: Semigroup.type) inline def derived[A]: Semigroup[A] = DerivedSemigroup.strict[A]
  extension (x: Monoid.type) inline def derived[A]: Monoid[A] = DerivedMonoid.strict[A]

  object semiauto:
    inline def eq[A]: Eq[A] = DerivedEq.strict[A]
    inline def hash[A]: Hash[A] = DerivedHash.strict[A]
    inline def empty[A]: Empty[A] = DerivedEmpty.strict[A]
    inline def semigroup[A]: Semigroup[A] = DerivedSemigroup.strict[A]
    inline def monoid[A]: Monoid[A] = DerivedMonoid.strict[A]

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

  object order:
    inline given [A](using NotGiven[Order[A]]): Order[A] = DerivedOrder[A]

  object commutativeSemigroup:
    inline given [A](using NotGiven[CommutativeSemigroup[A]]): CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]

  object commutativeMonoid:
    inline given [A](using NotGiven[CommutativeMonoid[A]]): CommutativeMonoid[A] = DerivedCommutativeMonoid[A]

  object show:
    inline given [A](using NotGiven[Show[A]]): Show[A] = DerivedShow[A]

  object applicative:
    inline given [F[_]](using NotGiven[Applicative[F]]): Applicative[F] = DerivedApplicative[F]

  object apply:
    inline given [F[_]](using NotGiven[Apply[F]]): Apply[F] = DerivedApply[F]

  object emptyK:
    inline given [F[_]](using NotGiven[EmptyK[F]]): EmptyK[F] = DerivedEmptyK[F]

  object pure:
    inline given [F[_]](using NotGiven[Pure[F]]): Pure[F] = DerivedPure[F]

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

  object semigroupK:
    inline given [F[_]](using NotGiven[SemigroupK[F]]): SemigroupK[F] = DerivedSemigroupK[F]

  object monoidK:
    inline given [F[_]](using NotGiven[MonoidK[F]]): MonoidK[F] = DerivedMonoidK[F]

  object contravariant:
    inline given [F[_]](using NotGiven[Contravariant[F]]): Contravariant[F] = DerivedContravariant[F]

  object invariant:
    inline given [F[_]](using NotGiven[Invariant[F]]): Invariant[F] = DerivedInvariant[F]

  object partialOrder:
    inline given [A](using NotGiven[PartialOrder[A]]): PartialOrder[A] = DerivedPartialOrder[A]

  object showPretty:
    inline given [A](using NotGiven[Show[A]]): ShowPretty[A] = DerivedShowPretty[A]
