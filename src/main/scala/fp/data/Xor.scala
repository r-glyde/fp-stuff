package fp.data

import fp.data.Xor.{XLeft, XRight}

sealed trait Xor[+A, +B] {
  def map[C](f: B => C): Xor[A, C] = this match {
    case XLeft(_)  => this.asInstanceOf[Xor[A, C]]
    case XRight(r) => XRight(f(r))
  }

  def flatMap[C, D](f: B => Xor[C, D]): Xor[C, D] = this match {
    case XLeft(_)  => this.asInstanceOf[Xor[C, D]]
    case XRight(r) => f(r)
  }

  def isLeft: Boolean = this match {
    case _: XLeft[A]  => true
    case _: XRight[B] => false
  }

  def isRight: Boolean = !isLeft

  def getOrElse[BB >: B](default: => BB): BB = this match {
    case XLeft(_)  => default
    case XRight(r) => r
  }

  def orElse[AA >: A, BB >: B](alt: => Xor[AA, BB]): Xor[AA, BB] = if (isLeft) alt else this
}

object Xor {
  case class XLeft[A](l: A) extends Xor[A, Nothing]
  case class XRight[A](r: A) extends Xor[Nothing, A]
}
