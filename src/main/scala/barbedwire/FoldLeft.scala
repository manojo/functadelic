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
  with OrderingOps with NumericOps with PrimitiveOps with LiftVariables with While
  /*with TupleOps with HashMapOps*/ {

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

    /**
     * concat
     */
    def concat(that: FoldLeft[A, S]) = FoldLeft[A, S] { (z: Rep[S], comb: Comb[A, S]) =>
      val folded: Rep[S] = this.apply(z, comb)
      that.apply(folded, comb)
    }

    def ++(that: FoldLeft[A, S]) = this concat that

    /**
     * append
     */
    def append(elem: Rep[A]) = FoldLeft[A, S] { (z: Rep[S], comb: Comb[A, S]) =>
      val folded: Rep[S] = this.apply(z, comb)
      comb(folded, elem)
    }

    def :+(elem: Rep[A]) = this append elem

    /**
     * partition
     * This will create code what will run through the original fold twice
     * once for the positive predicate, once for the negative.
     */
    def partition(p: Rep[A] => Rep[Boolean]): (FoldLeft[A, S], FoldLeft[A, S]) = {
      val trues = this filter p
      val falses = this filter (a => !p(a))
      (trues, falses)
    }

    /**
    def partitionBis(p: Rep[A] => Rep[Boolean]): (FoldLeft[A, S], FoldLeft[A, S]) = {
      //ghost foldLefts
      val trues = FoldLeft[A, S] { (z: Rep[S], comb: Comb[A, S]) => z }
      val falses = FoldLeft[A, S] { (z: Rep[S], comb: Comb[A, S]) => z }

      this.apply(
        (trues, falses),
        (acc, elem) => if (p(elem)) (acc._1 :+ elem, acc._2) else (acc._1, acc._2 :+ elem)
      )
    }
    */


    /**
     * groupBy
     * What if HashMaps are also foldLefts?
     * In that case, the type of elements passing through them are key-value pairs,
     * of type [K, FoldLeft[A, S2]]. That's right, the values are also foldLefts, let's
     * hope we get rid of them later. We need a different sink type S2 for the inner values.
     * When we call `groupBy`, we specify what this guy will be.
     *
     * The outer sink type remains the same because it is eventually ``specialized''
     *
     *
     */
    /*
    def groupBy[K: Manifest, S2: Manifest](f: Rep[A] => Rep[K])(elemAt: (Rep[S], Rep[K]) => FoldLeft[A, S2]) =
      FoldLeft[(K, FoldLeft[A, S2]), S] { (z: Rep[S], comb: Comb[(K, FoldLeft[A, S2]), S]) =>

        this.apply(
          z,
          (acc: Rep[S], elem: Rep[A]) => {
            val k = f(elem)
            val fld = elemAt(acc, k)
            comb(acc, make_tuple2((k, fld :+ elem)))
          }
        )
    }*/
    /*
    def groupBy[K: Manifest, S2: Manifest](f: Rep[A] => Rep[K]) = FoldLeft[[K, FoldLeft[A, S2]], S] { (z: Rep[S], comb: Comb[[K, FoldLeft[A, S2]], S]) =>

      this.apply(
        z,
        (acc: Rep[S], elem: Rep[A]) => {
          val key: Rep[K] = f(elem)

          //type Comb[A, S] = (Rep[S], Rep[A]) => Rep[S]
          //type Comb[[K, FoldLeft[A, S2]], S] = (Rep[S], Rep[[K, FoldLeft[A, S2]]]) => Rep[S]



          comb(acc, (key, FoldLeft.create(elem, )))

        }
      )


      this.apply(Map[K, List[A]].empty, (acc: Map[K, List[A]], elem: A) => {
        val k = f(elem)
        acc + (k -> acc(k) ++ elem)
      })

      this.apply(Map[K, FoldLeft[A, S2]].empty, (acc: Map[K, FoldLeft[A, S2]], elem: A)) => {
        val k = f(elem)
        val fld: FoldLeft[A, S2] = acc(k)
        acc + (k -> fld append elem)
      }

      FoldLeft[[K, FoldLeft[A, S2]], S] = (z, comb)
      this.apply(z, (acc: S, elem: A)) => {
        val k = f(elem)
        val fld: FoldLeft[A, S2] = acc(k) <-- This line is messed up y'all!!!!
        comb(acc, fld append elem)
      }
      // add a function (S, K) => FoldLeft[A, S2]




      //val map =

    }
    */
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
