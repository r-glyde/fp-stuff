package examples

import fp.data.Maybe.Empty
import fp.data.{Maybe, MyList}
import fp.data.MyList._
import fp.instances.all._
import fp.syntax.all._

object Main extends App {

  // Maybe
  val a = Maybe(10)
  val b = 12.just

//  println(a.map(_ * 2))
//  println(a.flatMap(a => Maybe(a -100)))

  // Xor
  val r = "hello".asRight
  val l = 10.asLeft[String]

//  println(r.map(_.length))
//  println(l.map(_.length))
//  println(l.isLeft)

  // Ior
  val both = ("hello", 42).bothIor

//  println(both)
//  println(both.map(_ * 2))

  // List
  val listA = Link(1, Link(2, Link(3, Link(4, Link(5, End)))))
  val listB = Link(10, Link(20, Link(30, Link(40, Link(50, End)))))
  val listC = MyList(100, 200, 300, 400, 500)

//  println(listA)
//  println(listA.map(_ * 2))
//  println(listA.flatMap(x => Link(x, Link(2 * x, End))))
//  println(listA ++ listB)
//  println(0 +: listA)
//  println(listB :+ 60)

  // Semigroup

//  println("hello, " |+| "world")
//  println("hello, ".just |+| Empty)

  // Monoid

  // Functor
  println(r.fmap(_.length))
  println(both.fmap(_ * 2))
//  println(listC.fmap(_ * 2))
//  println(listC.as("hello"))
//  println(listC.void)

}
