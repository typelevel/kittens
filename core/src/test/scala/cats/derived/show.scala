package cats.derived
import cats.Show
import cats.derived.TestDefns.{Foo, Node, Tree, Leaf}
import MkShow._

class ShowTests extends KittensSuite {

  test("for simple product") {
    import cats.instances.all._
    val foo = Foo(42, Option("Hello"))
    assert(Show[Foo].show(foo) == "Foo(i = 42, b = Some(Hello))")
  }

  test("for tree") {
    import cats.instances.all._
    val tree = Node(Leaf(1), Node(Node(Leaf(2), Leaf(3)), Leaf(4)))
    assert(Show[Tree[Int]].show(tree) ==
      "Node(l = Leaf(t = 1), r = Node(l = Node(l = Leaf(t = 2), r = Leaf(t = 3)), r = Leaf(t = 4)))")
  }

  test("existing Show instances in scope are respected") {

    implicit def showInt: Show[Foo] = new Show[Foo] {
      def show(foo: Foo) = ""
    }

    val foo = Foo(42, Option("Hello"))
    assert(Show[Foo].show(foo) == "")

  }
}
