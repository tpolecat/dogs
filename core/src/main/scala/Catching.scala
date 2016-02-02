package dogs

import cats.Eval
import cats.data.Xor

import java.lang.Throwable

import scala.Null
import scala.reflect.ClassTag

trait Catching[F[_]] {
  def catchOnly[A, T >: Null <: Throwable: ClassTag](fa: F[A]): F[T Xor A]
  def catchNonFatal[A](fa: F[A]): F[Throwable Xor A]
}

object Catching {

  implicit val IOCatching: Catching[IO] =
    new Catching[IO] {
      def catchOnly[A, T >: Null <: Throwable: ClassTag](fa: IO[A]): IO[T Xor A] =
        fa.catchOnly[T]
      def catchNonFatal[A](fa: IO[A]): IO[Throwable Xor A] =
        fa.catchNonFatal
    }

  // TODO: transformers

}