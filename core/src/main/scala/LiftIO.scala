package dogs

import cats.Monoid
import cats.data.{ Kleisli, OptionT, Xor, XorT, WriterT }

import scala.{ Option, Some }

trait LiftIO[F[_]]  {
  def liftIO[A](ioa: IO[A]): F[A]
}

object LiftIO {
  def apply[F[_]](implicit F: LiftIO[F]): LiftIO[F] = F

  implicit def optionTLiftIO[F[_]: LiftIO] =
    new LiftIO[OptionT[F, ?]] {
      def liftIO[A](ioa: IO[A]) = OptionT(LiftIO[F].liftIO(ioa.map(Some(_): Option[A])))
    }

  implicit def xorTLiftIO[F[_]: LiftIO, E] =
    new LiftIO[XorT[F, E, ?]] {
      def liftIO[A](ioa: IO[A]) = XorT(LiftIO[F].liftIO(ioa.map(Xor.right)))
    }

  implicit def kleisliLiftIO[F[_]: LiftIO, E] =
    new LiftIO[Kleisli[F, E, ?]] {
      def liftIO[A](ioa: IO[A]) = Kleisli(_ => LiftIO[F].liftIO(ioa))
    }

  implicit def writerTLiftIO[F[_]: LiftIO, W: Monoid] =
    new LiftIO[WriterT[F, W, ?]] {
      def liftIO[A](ioa: IO[A]) = WriterT(LiftIO[F].liftIO(ioa.map((Monoid[W].empty, _))))
    }

  // TODO: other transformers

}







