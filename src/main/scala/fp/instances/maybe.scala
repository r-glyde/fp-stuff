package fp.instances

import fp.data.Maybe
import fp.data.Maybe.{Empty, Just}
import fp.{Functor, Monoid, Semigroup}

object maybe extends maybe

trait maybe {

  implicit val maybeInstances: Functor[Maybe] = new Functor[Maybe] {
    override def fmap[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa.map(f)
  }

  implicit def maybeMonoid[A: Semigroup]: Monoid[Maybe[A]] = new Monoid[Maybe[A]] {
    override def zero: Maybe[A] = Empty
    override def combine(x: Maybe[A], y: Maybe[A]): Maybe[A] = (x, y) match {
      case (Just(a), Just(b))  => Just(Semigroup[A].combine(a, b))
      case (_: Just[A], Empty) => x
      case (Empty, _: Just[A]) => y
      case (Empty, Empty)      => Empty
    }
  }

}
