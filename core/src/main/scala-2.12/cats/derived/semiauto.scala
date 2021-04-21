package cats.derived

/** allows semi automatically derive each instance. The derivation might need help when
  * there are fields with a type constructor that comes with instances, e.g.
  * {{{
  * scala> case class Bar(a: String)
  * scala> case class Foo(bars: List[Bar])
  * scala> import cats.instances.all._
  *
  * scala> cats.derived.semiauto.show[Foo].show(Foo(List(Bar("a"))))
  * res1: String = Foo(bars = \$colon\$colon(head = Bar(a = a), tl\$access\$1 = Nil.type()))
  * }}}
  * Note that semi.show didn't respect the native `Show[List]` instance
  *
  * You could either derive a Bar instance first
  * {{{
  * scala> implicit val barShow = cats.derived.semi.show[Bar]
  *
  * scala> cats.derived.semiauto.show[Foo].show(Foo(List(Bar("a"))))
  * res2: String = Foo(bars = List(Bar(a = a)))
  * }}}
  *
  * Or you can take advantage of a controlled auto derivation
  * {{{
  *   scala> implicit val fooShow: Show[Foo] = { |
  *             import cats.derived.auto.show._  |
  *             cats.derived.semiauto.show       |
  *          }
  *  scala> Foo(List(Bar("a"))).show
  *  res3: String = Foo(bars = List(Bar(a = a)))
  * }}}
  */
object semiauto extends SemiAutoInstances
