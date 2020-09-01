package examples

import fp.data.Maybe.Empty
import fp.data.{Maybe, MyList}
import fp.data.Chain._
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
  val listA = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, End)))))
  val listB = Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, End)))))
  val listC = MyList(100, 200, 300, 400, 500)

//  println(listA)
//  println(listA.map(_ * 2))
//  println(listA.flatMap(x => Link(x, Link(2 * x, End))))
//  println(listA ++ listB)
//  println(0 +: listA)
//  println(listB :+ 60)

  // Chain
  val chainA = Link(Single(1), Link(Single(2), Link(Single(3), Link(Single(4), Single(5)))))
  val chainB = Link(Single(10), Link(Single(20), Link(Single(30), Link(Single(40), Single(50)))))
  val chainC = Link(Single(1),Link(Single(2),Link(Single(3),Link(Single(4), Link(Single(5), Single(6))))))

  println(chainA)
  println(0 +: chainA)
  println(chainA :+ 6)
  println(chainA ++ chainB)
  println((chainA :+ 6) == chainC)
  println(chainA.map(_ * 100))
  println(chainA.flatMap(i => Link(Single(i), Single(i * 2))))
  println(chainB.maybeHead)

  // Semigroup

//  println("hello, " |+| "world")
//  println("hello, ".just |+| Empty)

  // Monoid

  // Functor
//  println(r.fmap(_.length))
//  println(both.fmap(_ * 2))
//  println(listC.fmap(_ * 2))
//  println(listC.as("hello"))
//  println(listC.void)

}
