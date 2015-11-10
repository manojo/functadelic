package barbedwire
/**
 * Basic test suite for streams (as per stream fusion)
 */

import org.scalatest.FunSuite
import scala.annotation.tailrec

/**
 * Test suite for Streams
 */

class StreamSuite extends FunSuite with Streams {

  val first10 = (1 to 10).toList

  test("empty list streamed is empty") {
    assert(toStream(List[Int]()).unStream == Nil)
  }

  test("nonempty list streamed is nonempty") {
    assert(toStream(List(1)).unStream == List(1))
    assert(toStream(first10).unStream == first10)
  }

  test("mapping over streams is the same as mapping over lists") {
    assert(toStream(List[Int]()).map(_ * 2).unStream == Nil)
    assert(toStream(first10).map(_ * 2).unStream == first10.map(_ * 2))
  }

  test("filtering over streams is the same as filtering over lists") {
    assert {
      toStream(first10).filter(_ % 2 == 1).unStream ==
        first10.filter(_ % 2 == 1)
    }
  }

  test("flatMapping over streams is the same as flatMapping over lists") {

    val ls1 = toStream(List[Int]()).flatMap { i => enumFromTo(1, i) }.unStream
    assert(ls1 == Nil)

    val ls2 = toStream(first10).flatMap { i => enumFromTo(1, i) }.unStream
    assert(ls2 == first10.flatMap { i => (1 to i).toList })
  }

  test("zipping over streams is the same as zipping over lists") {

    val ls = toStream(first10).zip(toStream(first10)).map { case (a, b) => a + b }.unStream

    assert(ls == (first10 zip first10).map { case (a, b) => a + b })

  }

  test("taking a stream is the same as taking a list") {
    val ls0 = List[Int]()
    val ls2 = 1 :: 2 :: Nil

    assert(ls0.take(3) == toStream(ls0).take(3).unStream)
    assert(first10.take(3) == toStream(first10).take(3).unStream)
    assert(ls2.take(3) == toStream(ls2).take(3).unStream)
  }

  test("windowing of elements by 3") {
    assert(listWindow3Bis(Nil) == listWindow3(Nil))
    assert(listWindow3Bis(first10) == listWindow3(first10))

    assert(listWindow3Bis(Nil) == listWindow3Tris(Nil))
    assert(listWindow3Tris(first10) == listWindow3(first10))

    assert(listWindow3Tris(first10) == streamWindow3(toStream(first10)).unStream)
    assert(listWindow3Tris(Nil) == streamWindow3(toStream(Nil)).unStream)
  }

  def listWindow3Bis(ls: List[Int]): List[List[Int]] = ls.splitAt(3) match {
    case (Nil, _) => Nil
    case (xs, rest) => xs :: listWindow3Bis(rest)
  }

  def listWindow3Tris(ls: List[Int]): List[List[Int]] = {

    @tailrec
    def loop(acc: List[List[Int]], tmpStream: List[Int]): List[List[Int]] = tmpStream.splitAt(3) match {
      case (Nil, _) => acc
      case (xs, rest) => loop(acc ++ (xs :: Nil), rest)
    }

    loop(Nil, ls)
  }

  def listWindow3(ls: List[Int]): List[List[Int]] = {
    @tailrec
    def loop(tmpStr: List[Int], tmpRes: List[List[Int]]): List[List[Int]] = tmpStr match {
      case Nil => tmpRes
      case _ => innerLoop(tmpStr, Nil, 3) match {
        case (Nil, res) => tmpRes ++ (res :: Nil)
        case (y, res) => loop(y, tmpRes ++ (res :: Nil))
      }
    }

    @tailrec
    def innerLoop(tmpStream: List[Int], tmpInner: List[Int], iter: Int): (List[Int], List[Int])
        = tmpStream match {

      case Nil => (tmpStream, tmpInner)
      case x :: xs =>
        if (iter <= 0) (tmpStream, tmpInner)
        else innerLoop(xs, tmpInner ++ List(x), iter - 1)
    }

    loop(ls, Nil)
  }

  def streamWindow3(str: Stream[Int]): Stream[List[Int]] = new Stream[List[Int]] {

    type S = (Option[str.S], List[Int], Int)
    def seed = (Some(str.seed), Nil, 3)

    def stepper(s: S) = s match {

      case (None, Nil, _) => Done()
      case (None, xs, i) => Yield(xs, (None, Nil, i))
      case (s1, xs, i) if (i <= 0) => Yield(xs, (s1, Nil, 3))
      case (Some(s1), xs, i) => str.stepper(s1) match {
        case Done() => Skip((None, xs, i))
        case Skip(rest) => Skip((Some(rest), xs, i))
        case Yield(elem, rest) => Skip((Some(rest), xs ++ List(elem), i - 1))
      }

    }
  }

}
