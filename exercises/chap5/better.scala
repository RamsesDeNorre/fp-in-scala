
object Chap5 {

  import Stream._

  sealed trait Stream[+A] {

    def fold [R] (nil: => R) (cons: (=> A, => R) => R) : R

    def isEmpty: Boolean =
      fold (true) ((_,_) => false)


    // ex 1
    
    def toList: List[A] =
      fold [List[A]] (Nil) (_::_)

    // ex 2

    def take : Int => Stream[A] = {
      case n if n <= 0 => empty
      case n           => fold [Stream[A]] (empty) ((h,t) => cons(h, t take (n-1)))
    }

    // ex 3

    def takeWhile (p: A => Boolean): Stream[A] =
      fold [Stream[A]] (empty) ((h,t) => if (p(h)) cons(h, t) else empty)

    // ex 4

    def forall (p: A => Boolean): Boolean =
      fold (true) ((h,t) => p(h) && t)

    // ex 6

    def map [B] (f: A => B): Stream[B] =
      fold [Stream[B]] (empty) ((h,t) => cons(f(h), t))

    def filter (p: A => Boolean): Stream[A] =
      fold [Stream[A]] (empty) ((h,t) => if (p(h)) cons(h,t) else t)

    def append [B >: A] (s: Stream[B]): Stream[B] =
      fold (s) (cons(_,_))

    def flatMap [B >: A] (f: A => Stream[B]): Stream[B] =
      fold [Stream[B]] (empty) ((h,t) => f(h) append t)

  }

  object Stream {

    def empty [A] : Stream[A] = new Stream[A] {
      override def fold [R] (nil: => R) (cons: (=> A, => R) => R) : R =
        nil
    }

    def cons [A] (hd: => A, tl: => Stream[A]): Stream[A] = new Stream[A] {
      override def fold [R] (nil: => R) (cons: (=> A, => R) => R) : R =
        cons(hd, tl.fold(nil)(cons))
    }

    def apply [A] (as: A*): Stream[A] =
      as.foldRight [Stream[A]] (empty) (cons(_,_))

    // ex 7

    def constant [A] (a: A): Stream[A] = cons(a, constant(a))

    // ex 8

    def from (n: Int): Stream[Int] = cons(n, from(n+1))

    // ex 9

    def fibs : Stream[Int] = {
      def fibs_ (n1: Int, n2: Int): Stream[Int] = {
        val s = n1 + n2
        cons(s, fibs_(s, n1))
      }
      cons(0, cons(1, fibs_(1,0)))
    }

    // ex 10

    implicit class OptionWithFold [T] (o: Option[T]) {
      def fold [R] (r: => R) (f: T => R) = (o map f) getOrElse r
    }

    def unfold [A,S] (z: S) (f: S => Option[(A,S)]): Stream[A] = {
      def unfold_ (s: S): Stream[A] =
        f(s).fold [Stream[A]] (empty) {case (a,s_) => cons(a, unfold_(s_))} 
      
      unfold_ (z)
    }

  }

  val ones: Stream[Int] = cons(1, ones)
  val lin : Stream[Int] = cons(1, lin map (_+1))

}

