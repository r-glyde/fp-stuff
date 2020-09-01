package fp.syntax

import fp.{Functor, Semigroup}
import fp.data.Ior.{IBoth, ILeft, IRight}
import fp.data.{Ior, Maybe, Xor}
import fp.data.Maybe.Just
import fp.data.Xor._

object all {

  implicit class GenOps[A](val a: A) extends AnyVal {
    def just: Maybe[A] = Just(a)
    def asLeft[B]: Xor[A, B] = XLeft[A](a)
    def asRight[B]: Xor[B, A] = XRight[A](a)
    def leftIor[B]: Ior[A, B] = ILeft(a)
    def rightIor[B]: Ior[B, A] = IRight(a)
  }

  implicit class GenPairOps[A, B](val pair: (A, B)) extends AnyVal {
    def bothIor: Ior[A, B] = IBoth(pair._1, pair._2)
  }

  implicit class SemigroupOps[A : Semigroup](val x: A) {
    def |+|(y: A): A = Semigroup[A].combine(x, y)
  }

  implicit class FunctorOps[F[_] : Functor, A](val fa: F[A]) {
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(fa)(f)
    def void: F[Unit] = Functor[F].void(fa)
    def as[B](b: B): F[B] = Functor[F].as(fa)(b)
  }

}
