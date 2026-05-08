package cats.derived

import cats.{Eq, Hash, Invariant, Order, PartialOrder, Show}
import cats.{Bifunctor, Bifoldable, Bitraverse}
import cats.{Foldable, NonEmptyTraverse, Reducible, Traverse}

object StackSafetySuite:
  import ADTs.*

  object eqInst:
    given Eq[IList[Int]] = semiauto.eq

  object orderInst:
    given Order[IList[Int]] = semiauto.order

  object pOrderInst:
    given PartialOrder[IList[Int]] = semiauto.partialOrder

  object hashInst:
    given Hash[IList[Int]] = semiauto.hash

  object showInst:
    given Show[IList[Int]] = semiauto.show

  object invariantInst:
    given Invariant[EnumK1] = semiauto.invariant

  object foldableInst:
    given Foldable[EnumK1] = semiauto.foldable

  object traverseInst:
    given Traverse[EnumK1] = semiauto.traverse

  object reducibleInst:
    given Reducible[EnumK1] = semiauto.reducible

  object netInst:
    given NonEmptyTraverse[EnumK1] = semiauto.nonEmptyTraverse

  enum BiTree[+A, +B]:
    case Leaf(a: A, b: B)
    case Branch(l: BiTree[A, B], r: BiTree[A, B])

  object BiTree:
    given Bifunctor[BiTree] = semiauto.bifunctor
    given Bifoldable[BiTree] = semiauto.bifoldable
    given Bitraverse[BiTree] = semiauto.bitraverse

class StackSafetySuite extends KittensSuite:
  import ADTs.*
  import StackSafetySuite.*

  val Size = 10000

  def deepIList(): IList[Int] =
    (1 to Size).foldLeft[IList[Int]](INil())((acc, i) => ICons(i, acc))

  def deepEnumK1(): EnumK1[Int] =
    (1 to Size).foldLeft[EnumK1[Int]](EnumK1.Leaf(0))((acc, i) => EnumK1.Rec(EnumK1.Leaf(i), acc))

  def deepBiTree(): BiTree[Int, Int] =
    (1 to Size).foldLeft[BiTree[Int, Int]](BiTree.Leaf(0, 0))((acc, i) => BiTree.Branch(BiTree.Leaf(i, i), acc))

  test("DerivedEq stack safety on IList[Int]"):
    import eqInst.given
    assert(Eq[IList[Int]].eqv(deepIList(), deepIList()))

  test("DerivedOrder stack safety on IList[Int]"):
    import orderInst.given
    assertEquals(Order[IList[Int]].compare(deepIList(), deepIList()), 0)

  test("DerivedPartialOrder stack safety on IList[Int]"):
    import pOrderInst.given
    assertEquals(PartialOrder[IList[Int]].partialCompare(deepIList(), deepIList()), 0.0)

  test("DerivedHash stack safety on IList[Int]"):
    import hashInst.given
    val h = Hash[IList[Int]].hash(deepIList())
    assert(h != Int.MinValue || h == Int.MinValue)

  test("DerivedShow stack safety on IList[Int]"):
    import showInst.given
    assert(Show[IList[Int]].show(deepIList()).length > 0)

  test("DerivedInvariant stack safety on EnumK1"):
    import invariantInst.given
    assert(Invariant[EnumK1].imap(deepEnumK1())(_ + 1)(_ - 1) ne null)

  test("DerivedFoldable.foldLeft stack safety on EnumK1"):
    import foldableInst.given
    val r = Foldable[EnumK1].foldLeft(deepEnumK1(), 0)(_ + _)
    assert(r != Int.MinValue || r == Int.MinValue)

  test("DerivedTraverse stack safety on EnumK1"):
    import traverseInst.given
    assert(Traverse[EnumK1].map(deepEnumK1())(_ + 1) ne null)

  test("DerivedReducible stack safety on EnumK1"):
    import reducibleInst.given
    val r = Reducible[EnumK1].reduceLeft(deepEnumK1())(_ + _)
    assert(r != Int.MinValue || r == Int.MinValue)

  test("DerivedNonEmptyTraverse stack safety on EnumK1"):
    import netInst.given
    assert(NonEmptyTraverse[EnumK1].map(deepEnumK1())(_ + 1) ne null)

  test("DerivedBifunctor stack safety on recursive BiTree"):
    assert(Bifunctor[BiTree].bimap(deepBiTree())(_ + 1, _ + 1) ne null)

  test("DerivedBifoldable stack safety on recursive BiTree"):
    val r = Bifoldable[BiTree].bifoldLeft(deepBiTree(), 0)(_ + _, _ + _)
    assert(r != Int.MinValue || r == Int.MinValue)

  test("DerivedBitraverse stack safety on recursive BiTree"):
    import cats.Eval
    val r = Bitraverse[BiTree].bitraverse[Eval, Int, Int, Int, Int](deepBiTree())(Eval.now(_), Eval.now(_)).value
    assert(r ne null)

end StackSafetySuite
