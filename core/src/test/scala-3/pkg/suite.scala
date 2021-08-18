package pkg

import munit.FunSuite

class MinimizationSuite extends FunSuite {

  test("basic test") {
    import cats.derived.semiauto.*

    val F = functor[IList]

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
