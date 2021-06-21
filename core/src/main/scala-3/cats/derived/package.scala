package cats.derived

import cats.*

extension (F: Foldable.type)
  inline def derived[F[_]]: Foldable[F] = DerivedFoldable[F]

extension (F: Functor.type)
  inline def derived[F[_]]: Functor[F] = DerivedFunctor[F]

extension (F: Reducible.type)
  inline def derived[F[_]]: Reducible[F] = DerivedReducible[F]

extension (F: Traverse.type)
  inline def derived[F[_]]: Traverse[F] = DerivedTraverse[F]

object semiauto extends
  CommutativeMonoidDerivation,
  CommutativeSemigroupDerivation,
  ContravariantDerivation,
  EmptyDerivation,
  EmptyKDerivation,
  EqDerivation,
  HashDerivation,
  InvariantDerivation,
  MonoidDerivation,
  MonoidKDerivation,
  OrderDerivation,
  PartialOrderDerivation,
  SemigroupDerivation,
  SemigroupKDerivation,
  ShowDerivation,
  Instances:

  inline def foldable[F[_]]: Foldable[F] = DerivedFoldable[F]

  inline def functor[F[_]]: Functor[F] = DerivedFunctor[F]

  inline def reducible[F[_]]: Reducible[F] = DerivedReducible[F]

  inline def traverse[F[_]]: Traverse[F] = DerivedTraverse[F]


object auto:
  object functor:
    inline given [F[_]]: Functor[F] = DerivedFunctor[F]

  object foldable:
    inline given [F[_]]: Foldable[F] = DerivedFoldable[F]

  object reducible:
    inline given [F[_]]: Reducible[F] = DerivedReducible[F]

  object traverse:
    inline given [F[_]]: Traverse[F] = DerivedTraverse[F]
