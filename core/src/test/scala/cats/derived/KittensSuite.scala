/*
 * Copyright (c) 2015 Miles Sabin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cats.derived

import cats.syntax.AllSyntax
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.Discipline

import scala.util.control.NonFatal

/**
 * An opinionated stack of traits to improve consistency and reduce
 * boilerplate in Kittens tests. Note that unlike the corresponding
 * CatsSuite in the Cat project, this trait does not mix in any
 * instances.
 */
trait KittensSuite extends AnyFunSuite with Discipline with AllSyntax with SerializableTest


trait SerializableTest {
  def isSerializable[T](a: T): Boolean = {
  import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream }
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    var ois: ObjectInputStream = null
    try {
      oos.writeObject(a)
      oos.close()
      val bais = new ByteArrayInputStream(baos.toByteArray())
      ois = new ObjectInputStream(bais)
      val a2 = ois.readObject()
      ois.close()
      true
    } catch { case NonFatal(t) =>
      throw new Exception(t)
    } finally {
      oos.close()
      if (ois != null) ois.close()
    }
  }
}
