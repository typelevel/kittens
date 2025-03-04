package cats.derived

import alleycats.Empty
import shapeless3.deriving.Derived
import shapeless3.deriving.K0.*

import scala.annotation.*
import scala.compiletime.*

@implicitNotFound("""Could not derive Empty for ${A}.
Make sure it satisfies one of the following conditions:
  * case class where all fields form Empty
  * sealed trait where exactly one subclass forms Empty
  * enum where exactly one variant forms Empty""")
type DerivedEmpty[A] = Derived[Empty[A]]
object DerivedEmpty:
  @nowarn("msg=unused import")
  inline def apply[A]: Empty[A] =
    import DerivedEmpty.given
    summonInline[DerivedEmpty[A]].instance

  @nowarn("msg=unused import")
  inline def strict[A]: Empty[A] =
    import Strict.given
    summonInline[DerivedEmpty[A]].instance

  given product[A](using inst: ProductInstances[Empty |: Derived, A]): DerivedEmpty[A] =
    Strict.product(using inst.unify)

  inline given coproduct[A: CoproductGeneric]: DerivedEmpty[A] =
    Strict.coproduct

  object Strict:
    given product[A: ProductInstancesOf[Empty]]: DerivedEmpty[A] =
      Empty(ProductInstances.construct([a] => (A: Empty[a]) => A.empty))

    @nowarn("id=E197")
    inline given coproduct[A: CoproductGeneric]: DerivedEmpty[A] =
      Empty(CoproductGeneric.withOnly[Empty |: Derived, A]([a <: A] => (A: (Empty |: Derived)[a]) => A.unify.empty))
