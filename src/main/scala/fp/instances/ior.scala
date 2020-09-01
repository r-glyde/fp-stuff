package fp.instances

import fp.Functor
import fp.data.Ior

object ior extends ior

trait ior {

  implicit def iorFunctor[Left]: Functor[({type R[A] = Ior[Left, A]})#R] = new Functor[({type R[A] = Ior[Left, A]})#R] {
    override def fmap[A, B](fa: Ior[Left, A])(f: A => B): Ior[Left, B] = fa.map(f)
  }

}
