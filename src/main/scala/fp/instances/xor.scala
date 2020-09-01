package fp.instances

import fp.Functor
import fp.data.Xor

object xor extends xor

trait xor {

  implicit def xorFunctor[Left]: Functor[({type R[A] = Xor[Left, A]})#R] = new Functor[({type R[A] = Xor[Left, A]})#R] {
    override def fmap[A, B](fa: Xor[Left, A])(f: A => B): Xor[Left, B] = fa.map(f)
  }

}
