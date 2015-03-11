package barbedwire
/**
 * Basic test suite for streams (as per stream fusion)
 */

import org.scalatest.FunSuite

/**
 * Test suite for Streams
 */

class StreamSuite extends FunSuite with Streams {

  test("empty list streamed is empty") {
    assert(toStream(List[Int]()).unStream == Nil)
  }

  test("nonempty list streamed is nonempty") {
    assert(toStream(List(1)).unStream == List(1))
    assert(toStream(List(1, 2, 3)).unStream == List(1, 2, 3))
  }

  test("mapping over streams is the same as mapping over lists") {
    assert(toStream(List[Int]()).map(_ * 2).unStream == Nil)
    assert(toStream(List(1, 2, 3)).map(_ * 2).unStream == List(1, 2, 3).map(_ * 2))
  }

  test("filtering over streams is the same as filtering over lists") {
    assert {
      toStream((1 to 10).toList).filter(_ % 2 == 1).unStream ==
        (1 to 10).toList.filter(_ % 2 == 1)
    }
  }

  test("flatMapping over streams is the same as flatMapping over lists") {
    val ls = (1 to 5).toList

    val ls1 = toStream(List[Int]()).flatMap { i => enumFromTo(1, i) }.unStream
    assert(ls1 == Nil)

    val ls2 = toStream(ls).flatMap { i => enumFromTo(1, i) }.unStream
    assert(ls2 == ls.flatMap { i => (1 to i).toList })
  }

  test("zipping over streams is the same as zipping over lists") {
    val ls = (1 to 10).toList
    val ls2 = (1 to 10).toList

    val ls3 = toStream(ls).zip(toStream(ls2)).map { case (a, b) => a + b }.unStream

    assert(ls3 == (ls zip ls2).map { case (a, b) => a + b })

  }

}
