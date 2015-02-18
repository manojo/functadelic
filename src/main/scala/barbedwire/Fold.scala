package barbedwire

/**
 * Buildable, which is `build` in
 * "a shortcut to deforestation"
 *
 * related post: http://manojo.github.io/2015/01/26/shortcut-fusion-part1
 */
abstract class Buildable[A, B] {
  def z: B
  def cons: (A, B) => B
}

class ListBuildable[A] extends Buildable[A, List[A]] {
  def z = List[A]()
  def cons = (x: A, xs: List[A]) => x :: xs
}

/**
 * Fun with `Fold`.
 */
trait Folds {

  /**
   * The classic notation for foldRight
   */
  def foldRight[A, B](z: B)(f: (A, B) => B)(ls: List[A]): B = ls match {
    case Nil => z
    case x :: xs => f(x, foldRight(z)(f)(xs))
  }

  /**
   * map with foldRight
   */
  def map[A, B](ls: List[A], f: A => B): List[B] = foldRight(List[B]()) {
    (x: A, acc: List[B]) => f(x) :: acc
  }(ls)

  /**
   * sum with foldRight
   */
  def sum(ls: List[Int]): Int = foldRight(0) {
    (x: Int, acc: Int) => x  + acc
  }(ls)

  /**
   * filter.
   */
  def filter[A](ls: List[A], p: A => Boolean): List[A] = foldRight(List[A]()) {
    (x: A, acc: List[A]) => if(p(x)) x :: acc else acc
  }(ls)

  /**
   * flatMap
   */
  def flatMap[A, B](ls: List[A], f: A => List[B]): List[B] = foldRight(List[B]()) {
    (x: A, acc: List[B]) => f(x) ++ acc
  }(ls)

  /**
   * concat
   */
  def concat[A](xs: List[A], ys: List[A]): List[A] = foldRight(ys){
    (x: A, acc: List[A]) => x :: acc
  }(xs)

  /**
   * foldLeft
   */
  def foldLeft[A, B](z: B)(f: (B, A) => B)(ls: List[A]): B = {
    val inner = foldRight((x: B) => x) {
      (x: A, acc: B => B) =>
        (y: B) => acc(f(y, x))
    }(ls)
    inner(z)
  }

  /**
   * zip
   */
  def zip[A, B](xs: List[A], ys: List[B])(implicit b: ListBuildable[(A, B)]): List[(A, B)] = (xs, ys) match {
    case (x :: xs, y :: ys) => b.cons((x, y), zip(xs, ys))
    case _ => b.z
  }

  /**
   * zip 2
   */
  def zip2[A, B](xs: List[A], ys: List[B]) = {
    val done = (zs: List[B]) => List[(A, B)]()

    val comp = (x: A, acc: List[B] => List[(A, B)]) => {
      zs: List[B] => zs match {
        case Nil => Nil
        case z :: zs2 => (x, z) :: acc(zs2)
      }
    }

    (foldRight(done)(comp)(xs)) (ys)
}



  /**
   * The build function, as per
   * "A short cut to deforestation"
   * Abstracts over the building of a list
   * A cons and a nil function
   * The general type is like this:
   *    def build[A, B](cons: (A, B) => B)(nil: B): B
   * we specialize B to List[A] here
   */
  //def build[A, B](g: ((A, B) => B) => B => B): B

  def from[A, B](a: Int, b: Int)(builder: Buildable[Int, B]): B =
    if (a > b) builder.z
    else builder.cons(a, from(a + 1, b)(builder))

}

trait Bla {
  trait ListAlgebra[A, B] {
    def nil: B
    def cons(a: A, b: B): B
  }

  //def build[A, B](f: ListAlgebra[A, B] => B): B

  class ListBuilder[A] extends ListAlgebra[A, List[A]] {
    def nil = List[A]()
    def cons(a: A, b: List[A]) = a :: b
  }

  def build[A](f: ListBuilder[A] => List[A]): List[A] = f(new ListBuilder[A])

  def zip[A, B](xs: List[A], ys: List[B]): List[(A, B)] = {
    val zipBuilder = (b: ListBuilder[(A, B)]) => (xs, ys) match {
      case (x :: xs, y :: ys) => b.cons((x, y), zip(xs, ys))
      case _ => b.nil
    }
    build(zipBuilder)
  }

  def foldRight[A, B](z: B)(f: (A, B) => B)(ls: List[A]): B = ls match {
    case Nil => z
    case x :: xs => f(x, foldRight(z)(f)(xs))
  }


  def map[A, B](ls: List[A], f: A => B): List[B] = {
    val mapBuilder = (b: ListBuilder[B]) =>
      foldRight(b.nil)((elem: A, acc) => b.cons(f(elem), acc))(ls)

    build(mapBuilder)
  }
}

object HelloFolds extends Folds {

  def main(args: Array[String]) {
    println("Hello Folds")

    val reverse = foldLeft(List[Int]()){
      (acc: List[Int], x: Int) => x :: acc
    } _

    println(reverse(List(1, 2, 3)))

    implicit val abListBuilder = new ListBuildable[(Int, Char)]

    println(zip(List(1, 2, 3), List('a', 'b', 'c')))
    println(zip2(List(1, 2, 3), List('a', 'b', 'c')))

    def dotproduct(xs: List[Int], ys: List[Int]): Int = {
      val zipped = xs zip ys
      val multiplied = zipped map { case (x, y) => x + y }
      multiplied.sum
    }

    def dotproduct2(xs: List[Int], ys: List[Int]): Int = {
      def loop(xs2: List[Int], ys2: List[Int], tmp: Int): Int = (xs2, ys2) match {
        case (x :: rest1, y :: rest2) => loop(rest1, rest2, tmp + x * y)
        case (Nil, _) => tmp
        case (_, Nil) => tmp
      }

      loop(xs, ys, 0)
    }
  }
}
