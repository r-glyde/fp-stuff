package fp.instances

import fp.data.MyList
import fp.data.MyList.End
import fp.{Functor, Monoid, Semigroup}

object mylist extends mylist

trait mylist {

  implicit val listFunctor: Functor[MyList] = new Functor[MyList] {
    override def fmap[A, B](fa: MyList[A])(f: A => B): MyList[B] = fa.map(f)
  }

  implicit def listMonoid[A: Semigroup]: Monoid[MyList[A]] = new Monoid[MyList[A]] {
    override def zero: MyList[A] = End
    override def combine(x: MyList[A], y: MyList[A]): MyList[A] = x ++ y
  }

}
