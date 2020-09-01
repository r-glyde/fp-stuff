package fp.data

import fp.data.MyList.{End, Cons}

sealed trait MyList[+A] {

  def +:[AA >: A](value: AA): MyList[AA] = Cons(value, this)

  def :+[AA >: A](value: AA): MyList[AA] = this match {
    case End              => Cons(value, End)
    case Cons(head, tail) => Cons(head, tail :+ value)
  }

  def ++[AA >: A](that: MyList[AA]): MyList[AA] = this concat that

  def concat[AA >: A](that: MyList[AA]): MyList[AA] = this match {
    case End              => that
    case Cons(head, tail) => Cons(head, tail.concat(that))
  }

  def map[B](f: A => B): MyList[B] = this match {
    case End              => End
    case Cons(head, tail) => Cons(f(head), tail.map(f))
  }

  def flatMap[B](f: A => MyList[B]): MyList[B] = this match {
    case End              => End
    case Cons(head, tail) => f(head) ++ tail.flatMap(f)
  }

  def foreach(f: A => Unit): Unit = this map f

  def isEmpty: Boolean = this match {
    case End        => true
    case _: Cons[A] => false
  }

  def nonEmpty: Boolean = !isEmpty

}

object MyList {
  case object End extends MyList[Nothing]
  case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

  def apply[A](as: A*): MyList[A] = as match {
    case Seq(h, t @ _*) => Cons(h, apply(t: _*))
    case Seq()          => End
  }

}
