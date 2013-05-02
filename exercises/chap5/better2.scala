
object Chap5 {

  import Stream._
  

  sealed trait Stream[+A] {

    def fold [R] (nil: => R) (cons: (=> A, => Stream[A]) => R) : R

    def foldr [R] (nil: => R) (cons: (=> A, => R) => R) : R =
      fold (nil) ((a,as) => cons(a, as.foldr (nil) (cons)))

    def isEmpty: Boolean =
      foldr (true) ((_,_) => false)

    def head : Option[A] =
      foldr [Option[A]] (None) ((a,_) => Some(a))

    def tail : Stream[A] =
      fold (empty[A]) ((_,b) => b)


    // ex 1
    
    def toList: List[A] =
      foldr [List[A]] (Nil) (_::_)

    // ex 2

    def take : Int => Stream[A] = n =>
      fold (empty[A]) ((h,t) =>
        if (n > 0) cons(h, t take (n-1))
        else       empty
    )

    // ex 3

    def takeWhile (p: A => Boolean): Stream[A] =
      foldr (empty[A]) ((h,t) => if (p(h)) cons(h, t) else empty)

    // ex 4

    def forall (p: A => Boolean): Boolean =
      foldr (true) ((h,t) => p(h) && t)

    // ex 6

    def map [B] (f: A => B): Stream[B] =
      foldr (empty[B]) ((h,t) => cons(f(h), t))

    def filter (p: A => Boolean): Stream[A] =
      foldr (empty[A]) ((h,t) => if (p(h)) cons(h,t) else t)

    def append [B >: A] (s: => Stream[B]): Stream[B] =
      foldr (s) (cons(_,_))

    def flatMap [B >: A] (f: A => Stream[B]): Stream[B] =
      foldr (empty[B]) ((h,t) => f(h) append t)

  }

  object Stream {

    def empty [A] : Stream[A] = new Stream[A] {
      override def fold [R] (nil: => R) (cons: (=> A, => Stream[A]) => R) : R =
        nil
    }

    def cons [A] (hd: => A, tl: => Stream[A]): Stream[A] = new Stream[A] {
      override def fold [R] (nil: => R) (cons: (=> A, => Stream[A]) => R) : R =
        cons(hd, tl)
    }

    def apply [A] (as: A*): Stream[A] =
      as.foldRight (empty[A]) (cons(_,_))

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

    def unfold [A,S] (z: S) (f: S => Option[(A,S)]): Stream[A] = {
      def unfold_ (s: S): Stream[A] =
        f(s).fold (empty[A]) {case (a,s_) => cons(a, unfold_(s_))} 
      
      unfold_ (z)
    }

    // ex 11

    def fibs_u : Stream[Int] = {
      val tail = unfold ((1,0)) {
        case (n1: Int, n2: Int) =>
          val s = n1 + n2
          Some((s, (s, n1)))
      }

      cons(0, cons(1, tail))
    }

    def from_u (n: Int) : Stream[Int] =
      unfold (n) (n => Some (n,n+1))

    def constant_u [A] (a: A) : Stream[A] =
      unfold (a) (a => Some (a,a))

    val ones_u = constant (1)

    // ex 12

    def map [A,B] (f: A => B) : Stream[A] => Stream[B] =
      unfold (_) (s => s.head.map(h => (f(h), s.tail)))

    def take [A] (n: Int) : Stream[A] => Stream[A] = s =>
      unfold ((n, s)) { case (n, s) =>
        for { h <- s.head
              if n > 0
            } yield (h, (n-1, s.tail))
      }
        
      def takeWhile [A] (p: A => Boolean) : Stream[A] => Stream[A] =
        unfold (_) ( s => for { h <- s.head
                                if p(h)
                              } yield (h, s.tail)
        )

      def zipWith [A,B,C] (f: A => B => C) : Stream[A] => Stream[B] => Stream[C] =
        s => t => unfold ((s, t)) { case (s, t) =>
          for { sh <- s.head
                th <- t.head
              } yield (f(sh)(th), (s.tail, t.tail))
        }

      def zip [A,B] : Stream[A] => Stream[B] => Stream[(A,B)] =
        zipWith (pair)

      def zipAllWith [A,B,C] (f: Option[A] => Option[B] => C) : Stream[A] => Stream[B] => Stream[C] =
        s => t => unfold ((s, t)) { case (s, t) =>
          (s.head, t.head) match {
            case (None, None) => None
            case (hs, ht)     => Some ((f (hs) (ht), (s.tail, t.tail)))
          }
        }

      def zipAll [A,B] : Stream[A] => Stream[B] => Stream[(Option[A],Option[B])] =
        zipAllWith (pair)
      
      // ex 13

      def startsWith [A] : Stream[A] => Stream[A] => Boolean =
        s => t => zipAll (s) (t) .takeWhile (_._2.isDefined) forall {
          sequence(_).fold (false) { case (a,b) => a == b }
        }

      // ex 14

      def tails [A] : Stream[A] => Stream[Stream[A]] =
        unfold (_) { s =>
          if (s.isEmpty) None
          else           Some ((s, s.tail))
        } append Stream(empty)

      // ex 15

      def scanr [A,B] (f: (A, B) => B) (z: B) : Stream[A] => Stream[B] =
        _.foldr (Stream(z)) ((a, bs) =>
          bs.head.fold (empty[B]) (b => cons(f(a, b), bs))
        )

  }

  def pair [A,B] : A => B => (A,B) = a => b => (a,b)

  def sequence [A,B] : ((Option[A], Option[B])) => Option[(A,B)] = {
    case (oa, ob) => for { a <- oa
                           b <- ob
                         } yield (a,b)
  }

  implicit class OptionWithFold [T] (o: Option[T]) {
    def fold [R] (r: => R) (f: T => R) = (o map f) getOrElse r
  }

  val ones: Stream[Int] = cons(1, ones)
  val lin : Stream[Int] = cons(1, lin map (_+1))

}

