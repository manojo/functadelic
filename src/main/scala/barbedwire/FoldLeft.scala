package barbedwire

import scala.virtualization.lms.common._


/**
 * An implementation of foldr/build-like fusion
 * as a staged interpreter inside LMS
 *
 * stuff is encoded as foldLeft instead
 *
 * see the following related post: http://manojo.github.io/2015/02/19/staging-foldleft/
 *
 * the type signature of foldLeft is
 *    def foldLeft[A, B](z: B, comb: (B, A) => A)(xs: List[A]) : B
 *
 */
trait FoldLefts extends ListOps with IfThenElse with BooleanOps with Variables
  with OrderingOps with NumericOps with PrimitiveOps with LiftVariables with While {

  /**
   * a type alias for the combination function for
   * foldLeft
   * `A` is the type of elements that pass through the fold
   * `S` is the type that is eventually computed
   */
  type Comb[A, S] = (Rep[S], Rep[A]) => Rep[S]

  /**
   * foldLeft is basically a pair of a zero value and a combination function
   */
  abstract class FoldLeft[A: Manifest, S: Manifest] extends ((Rep[S], Comb[A, S]) => Rep[S]) {

    /**
     * map
     */
    def map[B: Manifest](f: Rep[A] => Rep[B]) = FoldLeft[B, S] { (z: Rep[S], comb: Comb[B, S]) =>
      this.apply(
        z,
        (acc: Rep[S], elem: Rep[A]) => comb(acc, f(elem))
      )
    }

    /**
     * filter
     */
    def filter(p: Rep[A] => Rep[Boolean]) = FoldLeft[A, S] {  (z: Rep[S], comb: Comb[A, S]) =>
      this.apply(
        z,
        (acc: Rep[S], elem: Rep[A]) =>
          if (p(elem)) comb(acc, elem) else acc
      )
    }

    /**
     * flatMap
     */
    def flatMap[B: Manifest](f: Rep[A] => FoldLeft[B, S]) = FoldLeft[B, S] { (z: Rep[S], comb: Comb[B, S]) =>
      this.apply(
        z,
        (acc: Rep[S], elem: Rep[A]) => {
          val nestedFld = f(elem)
          nestedFld.apply(acc, comb)
        }
      )
    }
  }

  /**
   * companion object, makes it easier to
   * construct folds
   */
  object FoldLeft {

    /**
     * helper function for ease of use
     */
    def apply[A: Manifest, S: Manifest](f: (Rep[S], Comb[A, S]) => Rep[S]) = new FoldLeft[A, S] {
      def apply(z: Rep[S], comb: Comb[A, S]): Rep[S] = f(z, comb)
    }

    /**
     * create a fold from list
     */
    def fromList[A: Manifest, S: Manifest](ls: Rep[List[A]]) = FoldLeft[A, S] { (z: Rep[S], comb: Comb[A, S]) =>

      var tmpList = ls
      var tmp = z

      while (!tmpList.isEmpty) {
        tmp = comb(tmp, tmpList.head)
        tmpList = tmpList.tail
      }

      tmp
    }

    /**
     * create a fold from a range
     */
    def fromRange[S: Manifest](a: Rep[Int], b: Rep[Int]) = FoldLeft[Int, S] { (z: Rep[S], comb: Comb[Int, S]) =>

      var tmpInt = a
      var tmp = z

      while (tmpInt <= b) {
        tmp = comb(tmp, tmpInt)
        tmpInt = tmpInt + 1
      }

      tmp
    }

  }
}
