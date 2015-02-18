package barbedwire

/**
  * A scala implementation of streams described in the 'stream fusion' paper by
  * Duncan Coutts et al.
  *
  * see related post: http://manojo.github.io/2015/02/11/shortcut-fusion-part2
  */

trait Streams {

  /** ADT for a Stepper */
  abstract class Step[A,S]
  case class Done[A,S]() extends Step[A,S]
  case class Yield[A,S](a: A, s: S) extends Step[A,S]
  case class Skip[A,S](s: S) extends Step[A,S]

  /**
   * The stream class
   * @param A: the type of elements that the Stream sees
   */
  abstract class Stream[A]{ self =>

    /**
     * Type of the seed given to a stream
     */
    type S

    /**
     * The seed itself over which the stream "iterates"
     */
    def seed: S

    /**
     * The function that is used to step through the seed
     */
    def stepper(s: S): Step[A, S]

    /**
     * the map function for streams
     */
    def map[B](f: A => B): Stream[B] = new Stream[B] {

      type S = self.S
      def seed = self.seed

      def stepper(s: S) = self.stepper(s) match {
        case Done() => Done()
        case Yield(a, s2) => Yield(f(a), s2)
        case Skip(s) => Skip(s)
      }
    }

    /**
     * filter function for streams
     */
    def filter(p: A => Boolean): Stream[A] = new Stream[A] {

      type S = self.S
      def seed = self.seed

      def stepper(s: S) = self.stepper(s) match {
        case Done() => Done()
        case Yield(a, s2) => if (p(a)) Yield(a, s2) else Skip(s2)
        case Skip(s) => Skip(s)
      }
    }

    /**
     * The flatMap function
     */
    def flatMap[B](f: A => Stream[B]) = new Stream[B] {

      /**
       * the seed is composed of the original seed from A
       * and a possible stream representing temporary results
       * from flattening
       */
      type S = (self.S, Option[Stream[B]])
      def seed = (self.seed, None)

      def stepper(s: S) = s match {

        /**
         * The case when we have either exhausted the
         * elements from `self`, or are in an intermediate
         * state, where a new `A` can appear
         */
        case (s1, None) => self.stepper(s1) match {
          case Done() => Done()
          case Yield(a, s2) => Skip((s2, Some(f(a))))
          case Skip(s2) => Skip(s2, None)
        }

        /**
         * The case when we are iterating through the stream
         * yielded by applying `f`
         */
        case (s1, Some(str)) => str.stepper(str.seed) match {
          case Done() => Skip((s1, None))
          case Yield(b, s2) =>

            val newStream = new Stream[B] {
              type S = str.S
              def seed = s2
              def stepper(s: S) = str.stepper(s)
            }

            Yield(b, (s1, Some(newStream)))

          case Skip(s2) =>
            val newStream = new Stream[B] {
              type S = str.S
              def seed = s2
              def stepper(s: S) = str.stepper(s)
            }

            Skip(s1, Some(newStream))
        }
      }
    }

    /**
     * The zipper function
     * assumption is that both streams will eventually
     * yield the same number of values
     */
    def zip[B](b: Stream[B]) = new Stream[(A, B)] {

      type S = (self.S, b.S)
      def seed = (self.seed, b.seed)

      def stepper(s: S) = s match { case (sa, sb) =>

        (self.stepper(sa), b.stepper(sb)) match {
          case (Done(), Done()) => Done()
          case (Yield(a, sa2), Yield(b, sb2)) => Yield((a, b), (sa2, sb2))
          case (Yield(_, _), Skip(sb2)) => Skip((sa, sb2))
          case (Skip(sa2), Yield(_, _)) => Skip((sa2, sb))
        }
      }

    }

    /**
     * unfolding the contents of a stream into a list
     */
    def unStream: List[A] = {

      def unfold(s: S): List[A] = self.stepper(s) match {
        case Done() => Nil
        case Yield(a, s2) => a :: unfold(s2)
        case Skip(s2) => unfold(s2)
      }
      unfold(seed)
    }

  }

  /**
   * converting a list into a stream
   */
  def toStream[T](ls: List[T]) = new Stream[T] {

    type S = List[T]
    def seed = ls

    def stepper(xs: List[T]) = xs match {
      case Nil => Done()
      case (y :: ys) => Yield(y, ys)
    }
  }

  /**
   * enumerating values
   */
  def enumFromTo(a: Int, b: Int) = new Stream[Int] {
    type S = Int
    def seed = a

    def stepper(n: Int) =
      if (n <= b) Yield(n, n + 1)
      else Done()
  }
}
