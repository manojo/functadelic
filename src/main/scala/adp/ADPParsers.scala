package adp

/**
 * A signature has two type members
 *  - a base alphabet
 *  - a type that is "calculated": answer type
 */
trait Signature {
  type Alphabet // input type
  type Answer // output type
}

/**
 * A base implementation of ADP style parser combinators
 * See "A Discipline of Dynamic Programming over Sequence Data",
 * Giegerich et al. for more details
 */
trait BaseParsers { this: Signature =>

  /**
   * the type of the input
   */
  type Input = Array[Alphabet]

  /**
   * abstract method, should be specified later
   */
  def input: Input

  /**
   * a subword denotes a start and an end index
   * in a sequence
   */
  type Subword = (Int, Int)

  /*
   * A parser is a function from a subword to a list of results
   */
  sealed abstract class Parser[T] extends (Subword => List[T]) {

    /**
     * map: simply applies the function on all results
     */
    def map[U](f: T => U) = Parser[U] { subword => this(subword) map f }

    /**
     * or: yields all results from this, concatenated to all results of that
     */
    def | (that: Parser[T]) = Parser[T] { subword => this(subword) ++ that(subword) }

    /**
     * concat: computes the cross product of all possible combinations
     * of `this` and `that` over the subword length
     */
    def ~ [U](that: Parser[U]) = Parser[(T, U)] { case (i, j) =>
      for {
        k <- (i + 1 until j).toList
        left <- this(i, k)
        right <- that(k + 1, j)
      } yield (left, right)
    }

    /**
     * aggregate: run a function on all elements of a list
     */
    def aggregate(h: List[T] => List[T]) = Parser[T] { subword =>
      h(this(subword))
    }

    /**
     * filter
     */
    def filter(p: T => Boolean) = Parser[T] { subword => this(subword) filter p }
  }

  /**
   * Basic element matching function
   * parses exactly one character, i.e. a subword of length 1
   */
  def el = Parser[Alphabet] { case (i, j) => if (i + 1 == j) List(input(i)) else Nil }

  /**
   * parses the empty subword
   */
  def empty = Parser[Unit] { case (i, j) => if (i == j) List(()) else Nil }

  /**
   * companion object
   */
  object Parser {

    def apply[T](f: Subword => List[T]) = new Parser[T] {
      def apply(s: Subword) = f(s)
    }
  }
}
