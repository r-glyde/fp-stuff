package fp.data

import fp.data.Maybe.{Empty, Just}

sealed trait Maybe[+A] {
  def map[B](f: A => B): Maybe[B] = this match {
    case Just(a) => Maybe(f(a))
    case Empty   => Empty
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
    case Just(a) => f(a)
    case Empty   => Empty
  }

  def isEmpty: Boolean = this match {
    case Just(_) => false
    case Empty   => true
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Just(value) => value
    case Empty       => default
  }

  def orElse[B >: A](alt: => Maybe[B]): Maybe[B] = if (isEmpty) alt else this

}

object Maybe {
  case class Just[A](value: A) extends Maybe[A]
  case object Empty extends Maybe[Nothing]

  def apply[A](a: A): Maybe[A] = if (a == null) Empty else Just(a)
  def empty[A]: Maybe[A] = Empty
}
