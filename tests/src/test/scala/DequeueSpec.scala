package dogs

import cats._
import cats.syntax.all._
import cats.std.all._

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import scala.annotation.Annotation
import scala.collection.immutable.{Nil,List,::}
import scala.{annotation,Int,Option,None,Some}
import scala.Predef._
import java.lang.String

object DequeueSpec extends Properties("Dequeue") {
  import Dequeue._
  import Maybe._

  @annotation.tailrec
  def consL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
    case Nil => q
    case x :: xs => consL(xs, q cons x)
  }
  @annotation.tailrec
  def unconsL[A](q: Dequeue[A], acc: List[A]): List[A] = q uncons match {
    case NotThere() => acc
    case There((i, q)) => unconsL(q, i :: acc)
  }
  @annotation.tailrec
  def snocL[A](l: List[A], q: Dequeue[A]): Dequeue[A] = l match {
    case Nil => q
    case x :: xs => snocL(xs, q snoc x)
  }
  @annotation.tailrec
  def unsnocL[A](q: Dequeue[A], acc: List[A]): List[A] = q unsnoc match {
    case NotThere() => acc
    case There((i, q)) => unsnocL(q, i :: acc)
  }

  property("enqueue onto an empty q can be fetched from either end") = secure {
    val x = "xyzzy"
    val q = Dequeue.empty cons x
    q.uncons == There((x,EmptyDequeue())) &&
    q.unsnoc == There((x,EmptyDequeue()))
  }

  property("cons and then uncons") = forAll { (xs: List[Int]) =>
    val q = consL(xs, empty)
    val l = unconsL(q, Nil)

    xs == l
  }

  property("snoc and then unsnoc") = forAll { (xs: List[Int]) =>
    val q = snocL(xs, empty)
    val l = unsnocL(q, Nil)

    xs == l
  }

  property("cons and then unsnoc") = forAll { (xs: List[Int]) =>
    val q = consL(xs, empty)
    val l = unsnocL(q, Nil)

    xs == l.reverse
  }

  property("snoc and then uncons") = forAll { (xs: List[Int]) =>
    val q = snocL(xs, empty)
    val l = unconsL(q, Nil)

    xs == l.reverse
  }

  property("snoc then toLst") = forAll { (xs: List[Int]) =>
    val q = snocL(xs, empty)
    q.toLst == Lst.fromIterable(xs)
  }

  property("cons then toLst") = forAll { (xs: List[Int]) =>
    val q = consL(xs, empty)
    q.toLst == Lst.fromIterable(xs).reverse
  }

  property("snoc then toBackLst") = forAll { (xs: List[Int]) =>
    val q = snocL(xs, empty)
    q.toBackLst == Lst.fromIterable(xs).reverse
  }

  property("cons then toBackLst") = forAll { (xs: List[Int]) =>
    val q = consL(xs, empty)
    q.toBackLst == Lst.fromIterable(xs)
  }

  implicit def genQ[A: Arbitrary]: Arbitrary[Dequeue[A]] = Arbitrary(
    for {
      l <- arbitrary[List[A]]
      r <- arbitrary[List[A]]
    } yield consL(l, snocL(r, empty)))

  property("foldLeft") = forAll { (q: Dequeue[Int]) =>
    q.foldLeft[Lst[Int]](El())((xs,x) => Nel(x, xs)) === q.toBackLst
  }
  property("foldRight") = forAll { (q: Dequeue[Int]) =>
    q.foldRight[Lst[Int]](Eval.now(El()))((x,xs) => xs.map(xs => Nel(x,xs))).value === q.toLst
  }
}
