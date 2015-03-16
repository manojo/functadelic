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
abstract class EitherCPS[A, B] { self =>
  def map[C, D](lmap: A => C, rmap: B => D) = new EitherCPS[C, D] {
    def apply[X](lf: C => X, rf: D => X) = self.apply(
      (a: A) => lf(lmap(a)),
      (b: B) => rf(rmap(b))
    )
  }

  def apply[X](lf: A => X, rf: B => X): X
}

case class LeftCPS[A, B](a: A) extends EitherCPS[A, B] {
  def apply[X](left: A => X, right: B => X) = left(a)
}

case class RightCPS[A, B](b: B) extends EitherCPS[A, B] {
  def apply[X](left: A => X, right: B => X) = right(b)
}

/**
 * companion object, defining apply
 */
object EitherCPS {

  /**
   * partition on lists, using EitherCPS
   */
  def partition[A](p: A => Boolean)(ls: List[A]): List[EitherCPS[A, A]] =
    ls map (x => if (p(x)) LeftCPS[A, A](x) else RightCPS[A, A](x))

  def main(args: Array[String]) {
    println("Watchout!")

    val ls = (1 to 10).toList
    val partitioned = partition[Int](_ % 2 == 0)(ls)
    val mapped: List[EitherCPS[Double, Int]] =
      partitioned map (_ map (x => x.toDouble, y => y))

    //println(partition[Int](_ % 2 == 0)(ls))

  }

}
