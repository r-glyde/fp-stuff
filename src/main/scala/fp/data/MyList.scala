package fp.data

import fp.data.MyList.{End, Link}

sealed trait MyList[+A] {

  def +:[AA >: A](value: AA): MyList[AA] = Link(value, this)

  def :+[AA >: A](value: AA): MyList[AA] = this match {
    case End              => Link(value, End)
    case Link(head, tail) => Link(head, tail :+ value)
  }

  def ++[AA >: A](that: MyList[AA]): MyList[AA] = this concat that

  def concat[AA >: A](that: MyList[AA]): MyList[AA] = this match {
    case End              => that
    case Link(head, tail) => Link(head, tail.concat(that))
  }

  def map[B](f: A => B): MyList[B] = this match {
    case End              => End
    case Link(head, tail) => Link(f(head), tail.map(f))
  }

  def flatMap[B](f: A => MyList[B]): MyList[B] = this match {
    case End              => End
    case Link(head, tail) => f(head) ++ tail.flatMap(f)
  }

  def isEmpty: Boolean = this match {
    case End        => true
    case _: Link[A] => false
  }

  def nonEmpty: Boolean = !isEmpty

}

object MyList {
  case object End extends MyList[Nothing]
  case class Link[A](head: A, tail: MyList[A]) extends MyList[A]

  def apply[A](as: A*): MyList[A] = as match {
    case Seq(h, t @ _*) => Link(h, apply(t: _*))
    case Seq()          => End
  }

}
