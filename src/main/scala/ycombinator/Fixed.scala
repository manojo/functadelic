package ycombinator

/**
 * This is an attempt at implementing the fix combinator
 * using recursive types in Scala.
 * See the following related post: http://manojo.github.io/2014/09/03/understanding-the-fix-operator/
 */
trait Fixed {
  type U
  type T = U => U

  case class Rec(val f: Rec => T) {
    def apply(x: Rec): T = f(x)
  }

  /** the recursively typed definition of fix
   * fix = \f: T -> T.
   * (\x:(uA. A -> T) f (x x))(\x:(uA. A -> T) f (x x))
   *
   * the combinator works in a call-by-name semantics
   * in call-by-value, untyped, we have
   *
   * fix_cbv = \f. (\x. f (\y. f x x y)) (\x. f (\y. f x x y))
   *
   * if we try to infer the types for this expression, we get either that:
   * * y forces a recursive type, or a function type on T
   * * T being a recursive type will leak into the type of f
   * so let's hack it and force T to be a function type instead
   *
   */

  def fix(f: T => T): T = {

    def inner = (x: Rec) => {
      f(y => x(x)(y))
    }
    inner(Rec(inner))
  }
}

object FixCombinator extends Fixed {

  type U = Int

  //defining factorial with fix

  def g = (fact: Int => Int) => (n: Int) =>
    if (n == 0) 1 else n * fact(n - 1)

  def bla = fix(g)

  def main(args:Array[String]) {
    println("Hello Fixed")
    println(bla(3))
  }

}
