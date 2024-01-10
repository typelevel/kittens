package cats.derived

import alleycats.Empty
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive an instance of Empty[A] where A = ${A}.
Make sure that A satisfies one of the following conditions:
  * it is a case class where all fields have an Empty instance
  * it is a sealed trait where exactly one subclass has an Empty instance""")
type DerivedEmpty[A] = Derived[Empty[A]]
object DerivedEmpty:
  type Or[A] = Derived.Or[Empty[A]]

  @nowarn("msg=unused import")
  inline def apply[A]: Empty[A] =
    import DerivedEmpty.given
    summonInline[DerivedEmpty[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Empty[A] =
    import Strict.given
    summonInline[DerivedEmpty[A]].instance

  given product[A: ProductInstancesOf[Or]]: DerivedEmpty[A] = Strict.product(using ProductInstances.unify)
  inline given coproduct[A: CoproductGeneric]: DerivedEmpty[A] = Strict.coproduct

  object Strict:
    given product[A: ProductInstancesOf[Empty]]: DerivedEmpty[A] =
      Empty(ProductInstances.construct([a] => (A: Empty[a]) => A.empty))

    inline given coproduct[A: CoproductGeneric]: DerivedEmpty[A] =
      Empty(CoproductGeneric.withOnly[Or, A]([a <: A] => (A: Or[a]) => A.unify.empty))
