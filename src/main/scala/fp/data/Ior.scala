package fp.data

import fp.data.Ior.{IBoth, ILeft, IRight}

sealed trait Ior[+A, +B] {
  def map[C](f: B => C): Ior[A, C] = this match {
    case ILeft(_)    => this.asInstanceOf[Ior[A, C]]
    case IRight(r)   => IRight(f(r))
    case IBoth(l, r) => IBoth(l, f(r))
  }
}

object Ior {
  case class ILeft[A, B](l: A) extends Ior[A, B]
  case class IRight[A, B](r: B) extends Ior[A, B]
  case class IBoth[A, B](l: A, r: B) extends Ior[A, B]

  def left[A, B](l: A): Ior[A, B] = ILeft[A, B](l)
  def right[A, B](r: B) : Ior[A, B] = IRight[A, B](r)
  def both[A, B](l: A, r: B): Ior[A, B] = IBoth(l, r)
}
