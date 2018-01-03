# kittens: automatic type class derivation for Cats and generic utility functions

**kittens** is a Scala library which provides instances of type classes from the [Cats][cats] library for arbitrary
algebraic data types using [shapeless][shapeless]-based automatic type class derivation. It also provides some utility functions related to cats.Applicative such as lift, traverse and sequence to HList, Record and arbitrary parameter list.

![kittens image](http://plastic-idolatry.com/erik/kittens2x.png)

kittens is part of the [Typelevel][typelevel] family of projects. It is an Open Source project under the Apache
License v2, hosted on [github][source]. Binary artefacts will be published to the [Sonatype OSS Repository Hosting
service][sonatype] and synced to Maven Central.

It is available for Scala 2.11 and 2.12, and Scala.js.

To get started with SBT, simply add the following to your build.sbt file:

```Scala
libraryDependencies += "org.typelevel" %% "kittens" % "1.0.0-RC2"
```

[![Build Status](https://api.travis-ci.org/milessabin/kittens.png?branch=master)](https://travis-ci.org/milessabin/kittens)
[![Stories in Ready](https://badge.waffle.io/milessabin/kittens.png?label=Ready)](https://waffle.io/milessabin/kittens)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/milessabin/kittens)
[![Maven Central](https://img.shields.io/maven-central/v/org.typelevel/kittens_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/org.typelevel/kittens_2.11)

### Auto derived Examples



```scala

scala> import cats.implicits._, cats._

scala> case class Cat[Food](food: Food, foods: List[Food])
defined class Cat

scala> val cat = Cat(1, List(2, 3))
cat: Cat[Int] = Cat(1,List(2, 3))

```

#### Derive `Functor`

```scala
scala> implicit val fc = cats.derive.functor[Cat]
FC: cats.Functor[Cat] = cats.derived.MkFunctor2$$anon$4@1c60573f

scala> cat.map(_ + 1)
res0: Cat[Int] = Cat(2,List(3, 4))
```

#### Derive `Show`

```scala

scala> case class Address(street: String, city: String, state: String)
scala> case class ContactInfo(phoneNumber: String, address: Address)
scala> case class People(name: String, contactInfo: ContactInfo)

scala> val mike = People("Mike", ContactInfo("202-295-3928", Address("1 Main ST", "Chicago", "IL")))
scala> import cats._,cats.implicits._

scala> //existing Show instance for Address
scala> implicit val addressShow: Show[Address] = new Show[Address] {
          def show(a: Address) = s"${a.street}, ${a.city}, ${a.state}" 
       }

scala> implicit val peopleShow = derive.show[People] //auto derive Show for People

scala> mike.show
res0: String = People(name = Mike, contactInfo = ContactInfo(phoneNumber = 202-295-3928, address = 1 Main ST, Chicago, IL))

```
Note that in this example, the derivation auto derived all referenced class but still respect the existing instance in scope. 

### Sequence examples
Note that to run these examples you need on partial unification to overcome [SI-2712](https://github.com/scala/bug/issues/2712). An easy way to achieve that is to use this [sbt-plugin](https://github.com/fiadliel/sbt-partial-unification), add to your `project/plugs.sbt`:

```Scala
addSbtPlugin("org.lyranthe.sbt" % "partial-unification" % "1.1.0")
```


```scala
scala> import cats.implicits._, cats.sequence._
import cats.implicits._
import cats.sequence._

scala> val f1 = (_: String).length
f1: String => Int = <function1>

scala> val f2 = (_: String).reverse
f2: String => String = <function1>

scala> val f3 = (_: String).toFloat
f3: String => Double = <function1>

scala> val f = sequence(f1, f2, f3)
f: String => shapeless.::[Int,shapeless.::[String,shapeless.::[Float,shapeless.HNil]]] = <function1>

scala> f("42.0")
res0: shapeless.::[Int,shapeless.::[String,shapeless.::[Float,shapeless.HNil]]] = 4 :: 0.24 :: 42.0 :: HNil

//or generic over ADTs
scala>  case class MyCase(a: Int, b: String, c: Float)
defined class MyCase

scala>  val myGen = sequenceGeneric[MyCase]
myGen: cats.sequence.sequenceGen[MyCase] = cats.sequence.SequenceOps$sequenceGen@63ae3243

scala> val f = myGen(a = f1, b = f2, c = f3)
f: String => MyCase = <function1>

scala> f("42.0")
res1: MyCase = MyCase(4,0.24,42.0)

```

Traverse works similarly but you need a Poly.

### Lift examples

```scala
scala> import cats._, implicits._, lift._
import cats._
import implicits._
import lift._

scala> def foo(x: Int, y: String, z: Float) = s"$x - $y - $z"

scala> val lifted = Applicative[Option].liftA(foo _)
lifted: (Option[Int], Option[String], Option[Float]) => Option[String] = <function3>

scala> lifted(Some(1), Some("a"), Some(3.2f))
res0: Option[String] = Some(1 - a - 3.2)

```


[cats]: https://github.com/typelevel/cats
[shapeless]: https://github.com/milessabin/shapeless
[typelevel]: http://typelevel.org/
[source]: https://github.com/milessabin/kittens
[sonatype]: https://oss.sonatype.org/

### kittens and Typelevel Scala

[Typelevel Scala][tls] provides a [partial fix for SI-7046][si-7046-pr] which can present obstacles to the uses of
shapeless's `Generic` and `LabelledGeneric` for the sealed trait at the root of an ADT such as you find in Kittens. If
it appears that these two type classes are unable to find (all of) the subclasses of an ADT root trait then please try
using Typelevel Scala and see if it resolves the issue.

To use Typelevel Scala you should,

+ Update your `project/build.properties` to require SBT 0.13.13 or later,

  ```
  sbt.version=0.13.13
  ```

+ Add the following to your `build.sbt` immediately next to where you set `scalaVersion`,

  ```
  scalaOrganization := "org.typelevel"
  ```

If this does resolve the problem, please lend your support to the [pull request][si-7046-pr] being merged in Lightbend
Scala.

[tls]: https://github.com/typelevel/scala
[si-7046-pr]: https://github.com/scala/scala/pull/5284

## Participation

The Kittens project supports the [Typelevel][typelevel] [code of conduct][codeofconduct] and wants all of its
channels (mailing list, Gitter, github, etc.) to be welcoming environments for everyone.

[codeofconduct]: http://typelevel.org/conduct.html

## Building kittens

kittens is built with SBT 0.13.9 or later, and its master branch is built with Scala 2.11.7 by default.

## Contributors

+ Cody Allen <ceedubs@gmail.com> [@fourierstrick](https://twitter.com/fourierstrick)
+ Kailuo Wang <kailuo.wang@gmail.com> [@kailuowang](https://twitter.com/kailuowang)
+ Miles Sabin <miles@milessabin.com> [@milessabin](https://twitter.com/milessabin)
+ Georgi Krastev <joro.kr.21@gmail.com> [@Joro_Kr](https://twitter.com/joro_kr)
+ Fabio Labella <fabio.labella2@gmail.com> [@SystemFw]()
+ Your name here :-)
