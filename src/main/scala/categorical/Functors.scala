package categorical

/**
 * An implementation of (endo-)functors
 * Object -> Objects (types to types) -> A -> F[A]
 * Arrows -> Arrows (functions to functions)\
 *
 * see the following related post: http://manojo.github.io/2015/01/15/essence-of-fold
 */
trait Functor[F[_]] {

  /**
   * also known in categorical speak as Ff
   */
  def fmap[A, B](r: F[A], f: A => B): F[B]
}

/**
 * A church encoding of natural numbers, as a functor
 * The functor N is given as N(X) = 1 + X
 */

abstract class NatF[+T]
case object Zero extends NatF[Nothing]
case class Succ[+T](t: T) extends NatF[T]

/**
 * NatF is a (endo-)functor on the category of Scala types
 * It takes a type T to a type NatF[T]
 * it takes a function T => U to a function NatF[T] => NatF[U]
 * This is given by implementing an instance of the Functor typeclass
 */
object natFunctor extends Functor[NatF] {
  def fmap[A, B](r: NatF[A], f: A => B) = r match {
    case Zero => Zero
    case Succ(a) => Succ(f(a))
  }
}

/**
 * The base list functor
 * The functor L is given as L(X) = 1 + A x X, for a type A
 * Here we look at IntList
 */
abstract class IntListF[+T]
case object NilF extends IntListF[Nothing]
case class ConsF[+T](n: Int, t: T) extends IntListF[T]

/**
 * IntListF is a (endo-)functor on the category of Scala types
 * It takes a type T to a type IntListF[T]
 * it takes a function T => U to a function IntListF[T] => IntListF[U]
 * This is given by implementing an instance of the Functor typeclass
 */
object intListFunctor extends Functor[IntListF] {
  def fmap[A, B](r: IntListF[A], f: A => B) = r match {
    case NilF => NilF
    case ConsF(n, a) => ConsF(n, f(a))
  }
}

trait Categorical {

  /**
   * An F-Algebra is a pair (a, A), A \in category C, a : F A -> A, where F is a functor
   * In the category of Scala types, A is a "concrete" type, say for example Int
   * As F is a functor, we take evidence of this fact as a parameter
   * Update: we nowhere need the fact that F needs to be a functor as of yet. So no need
   * to carry that parameter
   */
  type Algebra[F[_], A] = F[A] => A

  /**
   * An algebra for the Nat functor, that computes the successor function
   */
  val intNatAlgebra: Algebra[NatF, Int] = (elem: NatF[Int]) => elem match {
    case Zero => 0
    case Succ(n) => n + 1
  }

  /**
   * (After defining Fix)
   * Now we can create the fix point type for the NatF function
   */
  type Nat = Fix[NatF]

  /**
   * Coming back to the story of algebras,
   * We can create an algebra for `NatF[Fix[NatF]]`, or, as we also call it,
   * `Fix[NatF]`
   */
  val fixNatAlgebra: Algebra[NatF, Nat] = (elem: NatF[Nat]) => Fix(elem)

  /**
   * Let's do that for lists as well
   */
  type IntList = Fix[IntListF]
  val fixIntListAlgebra: Algebra[IntListF, IntList] = (elem: IntListF[IntList]) => Fix(elem)

  /**
   * Why do we even care for `Fix`? It's too mind-bending a type.
   * Because
   * 1. we just reconstructed recursive types from our type system (aka, we did't use Scala's internal support for them)
   * 2. because Fix forms a very particular algebra, known as the initial algebra. From this algebra to any other algebra (a, A),
   *    there is a `unique` mapping. The type of this unique mapping is
   *        uniqMap: (F[Fix[F]] => Fix[F]) => (F[A] => A)
   *
   * Let's look at what `Fix[F]` means. It already represents the initial algebra *by itself*. Look again above to see it.
   * Because `Fix` is not only a type, but it represents a constructor, aka a function `F[Fix[F]] => Fix[F]`
   *
   * We can use the fact that this unique mapping exists. Given some algebra `F[A] => A`, we can implement a function from
   * to implement a function from `F[Fix[F]] => A`. Because, by definition, `F[Fix[F]] === Fix[F]`, in fact we can implement a
   * function from `Fix[F] => A`. This function is called `fold`.
   *
   *    fold: (F[A] => A) => Fix[A] => A
   *
   */

  def fold[F[_], A](alg: F[A] => A)(fx: Fix[F])(implicit ev: Functor[F]): A = {
    val lifted: F[Fix[F]] = fx.out
    val mapped: F[A] = ev.fmap(lifted, fold(alg) _)
    alg(mapped)
  }

  /**
   * So what is the big deal?
   * Well, `Fix` allows us to define recursive types.
   * And, interpreting the type signature of `fold` in this context, we get that
   * `fold` takes a recursive type to a value. Wait, fold captures evaluation over a recursive type!
   * And that is indeed the beauty of it. We can lift the implementation of many recursive functions
   * over recursive types to folds, and reason with folds now!
   *
   * Here are two examples!
   */

  def nat2Int(n: Nat): Int = fold(intNatAlgebra)(n)(natFunctor)

  /**
   * some convenience functions to create values of type `Nat`
   */
  val zero: Nat = Fix[NatF](Zero)
  def succ(n: Nat): Nat = Fix[NatF](Succ(n))

  /**
   * summing a list
   */
  def sumList(ls: IntList): Int = fold[IntListF, Int]{
    case NilF => 0
    case ConsF(n, x) => n + x
  }(ls)(intListFunctor)

  /**
   * some convenience functions to create values of type `Nat`
   */
  val emptyIntList: IntList = Fix[IntListF](NilF)
  def cons(n: Int, ls: IntList): IntList = Fix[IntListF](ConsF(n, ls))


}

/**
 * The fix point type constructor. Called Mu in general.
 * Let's call it Fix, so that things get less confusing
 * Rappel: the fixed-point combinator, called y, satisfies the following property
 * y (f) = f (y (f))
 *
 * Analogously for types: The fixed-point type combinator, called Fix, satisties the
 * following property.
 * Fix [F] = F[Fix[F]]
 * It's sorta implicit that F has to be higher-kinded
 *
 * Attempt A:
 *
 *    type Fix[F[_]] = F[Fix[F]]
 *
 * does not work because we cannot define a type alias cyclically
 *
 * But, if we create a class, which has a member `F[Fix[F]]`, called
 * `Fix[F]`, it works. Essentially, we are saying that any value of
 * type `F[Fix[F]]` has an alias, called `Fix[F]`.
 */
case class Fix[F[_]](out: F[Fix[F]])

object HiFunctors extends Categorical {
  def main(args: Array[String]) {
    println("Hi Functors")

    val bla: IntListF[IntListF[IntListF[Nothing]]] = ConsF(2, ConsF(3, NilF))

    println(nat2Int(succ(succ(succ(zero)))))

    println(sumList(cons(3, cons(2, cons(1, emptyIntList)))))
  }
}
