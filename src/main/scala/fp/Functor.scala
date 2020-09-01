package fp

trait Functor[F[_]] {
  def fmap[A, B](fa: F[A])(f: A => B): F[B]

  def as[A, B](fa: F[A])(b: B): F[B] = fmap(fa)(_ => b)
  def void[A](fa: F[A]): F[Unit] = as(fa)(())
}

object Functor {
  def apply[F[_]](implicit ev: Functor[F]): Functor[F] = ev
}
