package cpsencodings

/**
 * A CPS encoding for `Either`
 * Essentially, `Either` is a function that takes a pair of functions (left, right)
 * that eventually return a type X.
 * This eventual type is materialized when we actually are interested in the result
 * `EitherCPS` therefore acts as a "passage" type.
 *
 * See example of partition for lists for an intuition.
 */
abstract class EitherCPS[A, B, X] extends ((A => X, B => X) => X) {
  def fold[C, D](lmap: A => C, rmap: B => D) = EitherCPS[C, D, X] {
    (l: C => X, r: D => X) => this.apply(
      (a: A) => l(lmap(a)),
      (b: B) => r(rmap(b))
    )
  }
}

case class LeftCPS[A, B, X](a: A) extends EitherCPS[A, B, X] {
  def apply(left: A => X, right: B => X) = left(a)
}

case class RightCPS[A, B, X](b: B) extends EitherCPS[A, B, X] {
  def apply(left: A => X, right: B => X) = right(b)
}

/**
 * companion object, defining apply
 */
object EitherCPS {

  def apply[A, B, X](f: (A => X, B => X) => X) = new EitherCPS[A, B, X] {
    def apply(left: A => X, right: B => X) = f(left, right)
  }

  /**
   * partition on lists, using EitherCPS
   */
  def partition[A](p: A => Boolean)(ls: List[A]): (List[A], List[A]) = {
    val tmp = ls.foldLeft(List[EitherCPS[A, A, A]]()) { (acc, elem) =>
      if (p(elem)) acc ++ List(LeftCPS[A, A, A](elem))
      else acc ++ List(RightCPS[A, A, A](elem))
    }

    tmp.foldLeft((List[A](), List[A]())) { case ((l, r), elem) => elem match {
      case e @ LeftCPS(a) => (l ++ List(e(a => a, b => b)), r)
      case e @ RightCPS(b) => (l, r ++ List(e(a => a, b => b)))
    }}
  }

  def main(args: Array[String]) {
    println("Watchout!")

    val ls = (1 to 10).toList
    println(partition[Int](_ % 2 == 0)(ls))

  }

}
