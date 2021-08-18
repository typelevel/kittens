package pkg

import cats.Functor
import munit.FunSuite

class MinimizationSuite extends FunSuite with IListDefn {

  test("basic test") {
    import cats.derived.auto.given

    val F = Functor[IList]

    val initial = ICons(1, INil())

    val f: Int => String = (i: Int) => (i * 2).toString
    val g: String => Long = (s: String) => s.toLong + 1

    assertEquals(F.map(initial)(identity), ICons(1, INil()))
    assertEquals(F.map(initial)(f), ICons("2", INil()))
    assertEquals(F.map(initial)(f), ICons("2", INil()))
    assertEquals(F.map(F.map(initial)(f))(g), ICons(3l, INil()))
    assertEquals(F.map(initial)(f.andThen(g)), ICons(3l, INil()))
  }

}
