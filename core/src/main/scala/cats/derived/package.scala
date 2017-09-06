package cats

/**
 * For backward compat purpose.
 * Use cats.derive to explicitly derive instance instead
 */
package object derived {
  object emptyK extends MkEmptyKDerivation
  object eq extends MkEqDerivation
  object foldable extends MkFoldableDerivation
  object functor extends MkFunctorDerivation
  object iterable extends IterableDerivationFromMkIterable
  object monoid extends MkMonoidDerivation
  object monoidK extends MkMonoidKDerivation
  object pure extends MkPureDerivation
  object semigroup extends MkSemigroupDerivation
  object semigroupK extends MkSemigroupKDerivation
  object show extends MkShowDerivation
}
