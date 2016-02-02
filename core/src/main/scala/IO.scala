package dogs

import cats.{ Eval, Monad, Show }
import cats.data.Xor

import java.lang.{ Throwable, String, Thread }

import scala.{ Console, Long, Null, Option, Some, Unit }
import scala.reflect.ClassTag

final class IO[A] private (private val eval: Eval[A]) {

  def unsafePerformIO: A =
    eval.value

  def catchOnly[T >: Null <: Throwable: ClassTag]: IO[T Xor A] =
    new IO(Eval.always(Xor.catchOnly[T](eval.value)))

  def catchNonFatal: IO[Throwable Xor A] =
    new IO(Eval.always(Xor.catchNonFatal(eval.value)))

  def map[B](f: A => B): IO[B] =
    new IO(eval.map(f))

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO(eval.flatMap(a => f(a).eval))

}

object IO extends IoInstances with IoPrimitives {

  /** Capture a side-effecting expression as a new IO primitive. */
  def newPrimitive[A](a: => A): IO[A] =
    new IO[A](Eval.always(a))

  def pure[A](a: A): IO[A] =
    new IO[A](Eval.now(a))

  def forkIO[A](ma: IO[A]): IO[Long] =
    IO.newPrimitive {
      val t = new Thread {
        setDaemon(false)
        override def run = { ma.unsafePerformIO; () }
      }
      t.start()
      t.getId()
    }

}

trait IoInstances {

  implicit val MonadIO: Monad[IO] =
    new Monad[IO] {
      def pure[A](a: A): IO[A] = IO.pure(a)
      def flatMap[A, B](ma: IO[A])(f: A => IO[B]): IO[B] = ma.flatMap(f)
      override def map[A, B](ma: IO[A])(f: A => B): IO[B] = ma.map(f)
    }

  implicit val LiftIOIO: LiftIO[IO] =
    new LiftIO[IO] {
      def liftIO[A](ioa: IO[A]) = ioa
    }

}

trait IoPrimitives { 

  def putStrLn(s: String): IO[Unit] =
    IO.newPrimitive(Console.println(s))

  def putLn[A](a: A)(implicit ev: Show[A]): IO[Unit] =
    IO.newPrimitive(Console.println(ev.show(a)))

}
