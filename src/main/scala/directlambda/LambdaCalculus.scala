package directlambda

/**
 *
 * t ::= x
 *     | \x. t
 *     | t t
 *
 * See the following related post: http://manojo.github.io/2015/01/11/lambda-calculus-direct-embedding
 */
object LambdaCalculus {

  /**
   * The type that represents
   * the recurvive lambda expression type
   *
   * The `apply` function represents application
   */

  abstract class D {
    def apply(d: D): D
  }

  /**
   * Creates a lambda abstraction
   */
  def lam(f: D => D) = new D {
    def apply(d: D) = f(d)
    override def toString = f.toString
  }

  /**
   * Creates a variable. Using the `Symbol` class
   * we can create symbols like `'x`
   */
  implicit def v(s: Symbol) = new D {
    def apply(d: D) = throw new Error("does not evaluate")
    override def toString = s.toString
  }

  val id: D = lam { x => x }

  /**
   * Boolean expressions
   */
  val tru: D = lam { x => lam { y => x } }
  val fls: D = lam { x => lam { y => y } }

  val and = lam { x => lam { y => x(y)(fls) } }
  val or = lam { x => lam { y => x(tru)(y) } }

  /**
   * Church numerals
   */

  val zero: D = lam { s => lam { z => z } }
  val succ: D = lam { n => lam { s => lam { z => s(n)(s)(z) } } }

  def main(args: Array[String]) {
    println(id('v))

    println(and(tru)(tru)('x)('y))
    println(or(fls)(fls)('x)('y))

  }

}
