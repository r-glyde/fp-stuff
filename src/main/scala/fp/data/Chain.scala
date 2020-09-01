package fp.data

import fp.data.Chain.{Last, Link, Single}
import fp.data.Maybe.{Empty, Just}

sealed trait Chain[+A] {

  def maybeHead: Maybe[A] = this match {
    case Single(value) => Just(value)
    case Link(head, _) => head.maybeHead
    case Chain.Last    => Empty
  }

  def +:[AA >: A](value: AA): Chain[AA] = this match {
    case Last => Single(value)
    case _    => Link(Single(value), this)
  }

  def :+[AA >: A](value: AA): Chain[AA] = this match {
    case Last => Single(value)
    case _    => Link(this, Single(value))
  }

  def ++[AA >: A](that: Chain[AA]): Chain[AA] = this concat that

  def concat[AA >: A](that: Chain[AA]): Chain[AA] = this match {
    case Last => that
    case _    => Link(this, that)
  }

  def map[B](f: A => B): Chain[B] = this match {
    case Single(a)  => Single(f(a))
    case Link(h, t) => Link(h.map(f), t.map(f))
    case Last       => Last
  }

  def flatMap[B](f: A => Chain[B]): Chain[B] = this match {
    case Single(a)  => f(a)
    case Link(h, t) => Link(h.flatMap(f), t.flatMap(f))
    case Last       => Last
  }

  def foreach(f: A => Unit): Unit = this map f

  override def toString: String = {
    val str = new StringBuilder("Chain(")
    this.foreach(a => str ++= s"$a, ")
    str + ")"
  }

}

object Chain {
  case class Single[+A](value: A) extends Chain[A]
  case class Link[+A](head: Chain[A], tail: Chain[A]) extends Chain[A]
  case object Last extends Chain[Nothing]
}
