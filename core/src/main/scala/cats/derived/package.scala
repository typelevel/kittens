package cats

/**
 * For backward compat purpose.
 * Use cats.derive to explicitly derive instance instead
 */
package object derived {
  @deprecated("use cats.derive.emptemptyy instead", "1.0.0-RC1")
  object empty extends MkEmptyDerivation

  object emptyK extends MkEmptyKDerivation

  @deprecated("use cats.derive.eq instead", "1.0.0-RC1")
  object eq extends MkEqDerivation

  object foldable extends MkFoldableDerivation

  @deprecated("use cats.derive.functor instead", "1.0.0-RC1")
  object functor extends MkFunctorDerivation

  object iterable extends IterableDerivationFromMkIterable

  @deprecated("use cats.derive.monoid instead", "1.0.0-RC1")
  object monoid extends MkMonoidDerivation

  @deprecated("use cats.derive.monoidK instead", "1.0.0-RC1")
  object monoidK extends MkMonoidK0

  object pure extends MkPureDerivation

  @deprecated("use cats.derive.semigroup instead", "1.0.0-RC1")
  object semigroup extends MkSemigroupDerivation

  @deprecated("use cats.derive.semigroupK instead", "1.0.0-RC1")
  object semigroupK extends MkSemigroupK0

  @deprecated("use cats.derive.show instead", "1.0.0-RC1")
  object show extends MkShowDerivation
}
