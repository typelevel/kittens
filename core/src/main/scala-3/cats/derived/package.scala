package cats.derived

import alleycats.*
import cats.*
import cats.kernel.{Band, CommutativeGroup, CommutativeMonoid, CommutativeSemigroup, Semilattice}

import scala.util.NotGiven

extension (x: Eq.type) inline def derived[A]: Eq[A] = DerivedEq[A]
extension (x: Hash.type) inline def derived[A]: Hash[A] = DerivedHash[A]
extension (x: Empty.type) inline def derived[A]: Empty[A] = DerivedEmpty[A]
extension (x: Semigroup.type) inline def derived[A]: Semigroup[A] = DerivedSemigroup[A]
extension (x: Monoid.type) inline def derived[A]: Monoid[A] = DerivedMonoid[A]
extension (x: Group.type) inline def derived[A]: Group[A] = DerivedGroup[A]
extension (x: Band.type) inline def derived[A]: Band[A] = DerivedBand[A]
extension (x: Order.type) inline def derived[A]: Order[A] = DerivedOrder[A]
extension (x: CommutativeSemigroup.type) inline def derived[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]
extension (x: CommutativeMonoid.type) inline def derived[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]
extension (x: CommutativeGroup.type) inline def derived[A]: CommutativeGroup[A] = DerivedCommutativeGroup[A]
extension (x: Semilattice.type) inline def derived[A]: Semilattice[A] = DerivedSemilattice[A]
extension (x: Show.type) inline def derived[A]: Show[A] = DerivedShow[A]
extension (x: Applicative.type) inline def derived[F[_]]: Applicative[F] = DerivedApplicative[F]
extension (x: Apply.type) inline def derived[F[_]]: Apply[F] = DerivedApply[F]
extension (x: NonEmptyAlternative.type) inline def derived[F[_]]: NonEmptyAlternative[F] = DerivedNonEmptyAlternative[F]
extension (x: Alternative.type) inline def derived[F[_]]: Alternative[F] = DerivedAlternative[F]
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
  inline def group[A]: Group[A] = DerivedGroup[A]
  inline def band[A]: Band[A] = DerivedBand[A]
  inline def order[A]: Order[A] = DerivedOrder[A]
  inline def commutativeSemigroup[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]
  inline def commutativeMonoid[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]
  inline def commutativeGroup[A]: CommutativeGroup[A] = DerivedCommutativeGroup[A]
  inline def semilattice[A]: Semilattice[A] = DerivedSemilattice[A]
  inline def applicative[F[_]]: Applicative[F] = DerivedApplicative[F]
  inline def apply[F[_]]: Apply[F] = DerivedApply[F]
  inline def nonEmptyAlternative[F[_]]: NonEmptyAlternative[F] = DerivedNonEmptyAlternative[F]
  inline def alternative[F[_]]: Alternative[F] = DerivedAlternative[F]
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
  extension (x: PartialOrder.type) inline def derived[A]: PartialOrder[A] = DerivedPartialOrder.strict[A]
  extension (x: Order.type) inline def derived[A]: Order[A] = DerivedOrder.strict[A]
  extension (x: Show.type) inline def derived[A]: Show[A] = DerivedShow.strict[A]
  extension (x: ShowPretty.type) inline def derived[A]: ShowPretty[A] = DerivedShowPretty.strict[A]
  extension (x: Empty.type) inline def derived[A]: Empty[A] = DerivedEmpty.strict[A]
  extension (x: Semigroup.type) inline def derived[A]: Semigroup[A] = DerivedSemigroup.strict[A]
  extension (x: Monoid.type) inline def derived[A]: Monoid[A] = DerivedMonoid.strict[A]
  extension (x: Group.type) inline def derived[A]: Group[A] = DerivedGroup.strict[A]
  extension (x: Band.type) inline def derived[A]: Band[A] = DerivedBand.strict[A]
  extension (x: CommutativeSemigroup.type)
    inline def derived[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup.strict[A]
  extension (x: CommutativeMonoid.type) inline def derived[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid.strict[A]
  extension (x: CommutativeGroup.type) inline def derived[A]: CommutativeGroup[A] = DerivedCommutativeGroup.strict[A]
  extension (x: Semilattice.type) inline def derived[A]: Semilattice[A] = DerivedSemilattice.strict[A]
  extension (x: EmptyK.type) inline def derived[F[_]]: EmptyK[F] = DerivedEmptyK.strict[F]
  extension (x: SemigroupK.type) inline def derived[F[_]]: SemigroupK[F] = DerivedSemigroupK.strict[F]
  extension (x: MonoidK.type) inline def derived[F[_]]: MonoidK[F] = DerivedMonoidK.strict[F]
  extension (x: Pure.type) inline def derived[F[_]]: Pure[F] = DerivedPure.strict[F]
  extension (x: Invariant.type) inline def derived[F[_]]: Invariant[F] = DerivedInvariant.strict[F]
  extension (x: Functor.type) inline def derived[F[_]]: Functor[F] = DerivedFunctor.strict[F]
  extension (x: Contravariant.type) inline def derived[F[_]]: Contravariant[F] = DerivedContravariant.strict[F]
  extension (x: Apply.type) inline def derived[F[_]]: Apply[F] = DerivedApply.strict[F]
  extension (x: Applicative.type) inline def derived[F[_]]: Applicative[F] = DerivedApplicative.strict[F]
  extension (x: NonEmptyAlternative.type)
    inline def derived[F[_]]: NonEmptyAlternative[F] = DerivedNonEmptyAlternative.strict[F]
  extension (x: Alternative.type) inline def derived[F[_]]: Alternative[F] = DerivedAlternative.strict[F]
  extension (x: Foldable.type) inline def derived[F[_]]: Foldable[F] = DerivedFoldable.strict[F]
  extension (x: Reducible.type) inline def derived[F[_]]: Reducible[F] = DerivedReducible.strict[F]
  extension (x: Traverse.type) inline def derived[F[_]]: Traverse[F] = DerivedTraverse.strict[F]
  extension (x: NonEmptyTraverse.type) inline def derived[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse.strict[F]

  object semiauto:
    inline def eq[A]: Eq[A] = DerivedEq.strict[A]
    inline def hash[A]: Hash[A] = DerivedHash.strict[A]
    inline def partialOrder[A]: PartialOrder[A] = DerivedPartialOrder.strict[A]
    inline def order[A]: Order[A] = DerivedOrder.strict[A]
    inline def show[A]: Show[A] = DerivedShow.strict[A]
    inline def showPretty[A]: ShowPretty[A] = DerivedShowPretty.strict[A]
    inline def empty[A]: Empty[A] = DerivedEmpty.strict[A]
    inline def semigroup[A]: Semigroup[A] = DerivedSemigroup.strict[A]
    inline def monoid[A]: Monoid[A] = DerivedMonoid.strict[A]
    inline def group[A]: Group[A] = DerivedGroup.strict[A]
    inline def band[A]: Band[A] = DerivedBand.strict[A]
    inline def commutativeSemigroup[A]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup.strict[A]
    inline def commutativeMonoid[A]: CommutativeMonoid[A] = DerivedCommutativeMonoid.strict[A]
    inline def commutativeGroup[A]: CommutativeGroup[A] = DerivedCommutativeGroup.strict[A]
    inline def semilattice[A]: Semilattice[A] = DerivedSemilattice.strict[A]
    inline def emptyK[F[_]]: EmptyK[F] = DerivedEmptyK.strict[F]
    inline def semigroupK[F[_]]: SemigroupK[F] = DerivedSemigroupK.strict[F]
    inline def monoidK[F[_]]: MonoidK[F] = DerivedMonoidK.strict[F]
    inline def pure[F[_]]: Pure[F] = DerivedPure.strict[F]
    inline def invariant[F[_]]: Invariant[F] = DerivedInvariant.strict[F]
    inline def functor[F[_]]: Functor[F] = DerivedFunctor.strict[F]
    inline def contravariant[F[_]]: Contravariant[F] = DerivedContravariant.strict[F]
    inline def apply[F[_]]: Apply[F] = DerivedApply.strict[F]
    inline def applicative[F[_]]: Applicative[F] = DerivedApplicative.strict[F]
    inline def nonEmptyAlternative[F[_]]: NonEmptyAlternative[F] = DerivedNonEmptyAlternative.strict[F]
    inline def alternative[F[_]]: Alternative[F] = DerivedAlternative.strict[F]
    inline def foldable[F[_]]: Foldable[F] = DerivedFoldable.strict[F]
    inline def reducible[F[_]]: Reducible[F] = DerivedReducible.strict[F]
    inline def traverse[F[_]]: Traverse[F] = DerivedTraverse.strict[F]
    inline def nonEmptyTraverse[F[_]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse.strict[F]

object auto:
  private type NotGivenA[F[_]] = [A] =>> NotGiven[F[A]]
  private type NotGivenF[T[_[_]]] = [F[_]] =>> NotGiven[T[F]]

  object eq:
    inline given [A: NotGivenA[Eq]]: Eq[A] = DerivedEq[A]

  object hash:
    inline given [A: NotGivenA[Hash]]: Hash[A] = DerivedHash[A]

  object empty:
    inline given [A: NotGivenA[Empty]]: Empty[A] = DerivedEmpty[A]

  object semigroup:
    inline given [A: NotGivenA[Semigroup]]: Semigroup[A] = DerivedSemigroup[A]

  object monoid:
    inline given [A: NotGivenA[Monoid]]: Monoid[A] = DerivedMonoid[A]

  object group:
    inline given [A: NotGivenA[Group]]: Group[A] = DerivedGroup[A]

  object band:
    inline given [A: NotGivenA[Band]]: Band[A] = DerivedBand[A]

  object order:
    inline given [A: NotGivenA[Order]]: Order[A] = DerivedOrder[A]

  object commutativeSemigroup:
    inline given [A: NotGivenA[CommutativeSemigroup]]: CommutativeSemigroup[A] = DerivedCommutativeSemigroup[A]

  object commutativeMonoid:
    inline given [A: NotGivenA[CommutativeMonoid]]: CommutativeMonoid[A] = DerivedCommutativeMonoid[A]

  object commutativeGroup:
    inline given [A: NotGivenA[CommutativeGroup]]: CommutativeGroup[A] = DerivedCommutativeGroup[A]

  object semilattice:
    inline given [A: NotGivenA[Semilattice]]: Semilattice[A] = DerivedSemilattice[A]

  object show:
    inline given [A: NotGivenA[Show]]: Show[A] = DerivedShow[A]

  object applicative:
    inline given [F[_]: NotGivenF[Applicative]]: Applicative[F] = DerivedApplicative[F]

  object apply:
    inline given [F[_]: NotGivenF[Apply]]: Apply[F] = DerivedApply[F]

  object nonEmptyAlternative:
    inline given [F[_]: NotGivenF[NonEmptyAlternative]]: NonEmptyAlternative[F] = DerivedNonEmptyAlternative[F]

  object alternative:
    inline given [F[_]: NotGivenF[Alternative]]: Alternative[F] = DerivedAlternative[F]

  object emptyK:
    inline given [F[_]: NotGivenF[EmptyK]]: EmptyK[F] = DerivedEmptyK[F]

  object pure:
    inline given [F[_]: NotGivenF[Pure]]: Pure[F] = DerivedPure[F]

  object functor:
    inline given [F[_]: NotGivenF[Functor]]: Functor[F] = DerivedFunctor[F]

  object foldable:
    inline given [F[_]: NotGivenF[Foldable]]: Foldable[F] = DerivedFoldable[F]

  object reducible:
    inline given [F[_]: NotGivenF[Reducible]]: Reducible[F] = DerivedReducible[F]

  object traverse:
    inline given [F[_]: NotGivenF[Traverse]]: Traverse[F] = DerivedTraverse[F]

  object nonEmptyTraverse:
    inline given [F[_]: NotGivenF[NonEmptyTraverse]]: NonEmptyTraverse[F] = DerivedNonEmptyTraverse[F]

  object semigroupK:
    inline given [F[_]: NotGivenF[SemigroupK]]: SemigroupK[F] = DerivedSemigroupK[F]

  object monoidK:
    inline given [F[_]: NotGivenF[MonoidK]]: MonoidK[F] = DerivedMonoidK[F]

  object contravariant:
    inline given [F[_]: NotGivenF[Contravariant]]: Contravariant[F] = DerivedContravariant[F]

  object invariant:
    inline given [F[_]: NotGivenF[Invariant]]: Invariant[F] = DerivedInvariant[F]

  object partialOrder:
    inline given [A: NotGivenA[PartialOrder]]: PartialOrder[A] = DerivedPartialOrder[A]

  object showPretty:
    inline given [A: NotGivenA[Show]]: ShowPretty[A] = DerivedShowPretty[A]
