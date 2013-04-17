import scala.annotation.tailrec

object Chap5 {

  import Stream._

  sealed trait Stream[+A] {

    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty

    // ex 1

    def toList: List[A] = uncons match {
      case Some ((hd, tl)) => hd :: tl.toList
      case None            => Nil
    }

    def toList_ : List[A] = {
      @tailrec
      def toListRec (s: Stream[A]) (acc: List[A]) : List[A] = s.uncons match {
        case Some ((hd, tl)) => toListRec (tl) (hd :: acc)
        case None            => acc
      }

      toListRec(this)(Nil).reverse
    }

    // ex 2

    def take (n: Int): Stream[A] = uncons match {
      case Some ((hd, tl)) if n > 0 => cons (hd, tl take (n-1))
      case _                        => empty
    }

    // ex 3

    def takeWhile (p: A => Boolean): Stream[A] = uncons match {
      case Some ((hd, tl)) if p (hd) => cons (hd, tl takeWhile p)
      case _                         => empty
    }
    
    def foldRight [B] (z: => B) (f: (A, => B) => B): B = uncons match {
      case Some((h, t)) => f (h, t.foldRight (z) (f))
      case None         => z
    }
     
    def exists (p: A => Boolean): Boolean =
      foldRight (false) ((h, t) => p(h) || t)

    // ex 4

    def forall (p: A => Boolean): Boolean =
      foldRight (true) ((h, t) => p(h) && t)

    // ex 5
    def takeWhile_ (p: A => Boolean): Stream[A] =
      foldRight[Stream[A]] (empty) ((h, t) => if (p(h)) cons(h, t) else cons(h, empty))

    // ex 6

    def map [B] (f: A => B): Stream[B] =
      foldRight[Stream[B]] (empty) ((h, t) => cons (f(h), t))

    def filter (p: A => Boolean): Stream[A] =
      foldRight[Stream[A]] (empty) ((h, t) => if (p(h)) cons(h,t) else t)

    def append [B >: A] (s: Stream[B]): Stream[B] =
      foldRight (s) (cons(_,_))

    //def flatMap [B >; A] (f: A => Stream[B]): Stream[B] =
    //  foldRight (empty) ((h, t) 

    def map_ [B] (f: A => B): Stream[B] = {
      val outer = this
      new Stream[B] {
        override def uncons = outer.uncons match {
          case Some ((hd, tl)) => Some ((f(hd), tl map_ f))
          case None            => None
        }
      }
    }

  }

  object Stream {

    def empty[A]: Stream[A] =
      new Stream[A] { override def uncons = None }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        override def uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

  }

  val ones: Stream[Int] = cons(1, ones)
  val lin : Stream[Int] = cons(1, lin.map_ (_+1))

}

