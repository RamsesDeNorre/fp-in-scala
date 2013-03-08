
import scala.annotation.tailrec

object Chap3 {

  sealed trait List[+A] {
    def :: [B >: A] : B => List[B] = new ::(_, this)
    override def toString: String = this match {
      case Nil   => "Nil"
      case x::xs => x.toString + " :: " + xs.toString
    }
  }
  case object Nil extends List[Nothing]
  case class ::[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def sum(ints: List[Int]): Int = ints match {
      case Nil   => 0
      case x::xs => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil       => 1.0
      case 0.0 :: _  => 0.0
      case x   :: xs => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else as.head :: apply(as.tail: _*)

    val example = 1 :: 2 :: 3 :: Nil
    val example2 = List(1,2,3)
    val total = sum(example)

    def curry [A,B,C] (f: (A,B) => C) (a: A) (b: B) : C =
      f (a, b)

    def tail [A] : List[A] => List[A] = {
      case Nil   => Nil
      case x::xs => xs
    }

    @tailrec
    def drop [A] (n: Int) (xs: List[A]) : List[A] =
      (n,xs) match {
        case (_, Nil)     => Nil
        case (0, xs)      => xs
        case (n, x::xs_)  => drop (n-1) (xs_)
      }

    def reverse [A] : List[A] => List[A] = {
      @tailrec
      def reverse_ [A] (acc: List[A]) (xs: List[A]) : List[A] = 
        xs match {
          case Nil    => acc
          case x::xs_ => reverse_ (x::acc) (xs_)
        }

      reverse_ (Nil)
    }

    def dropWhile [A] (p: A => Boolean) : List[A] => List[A] = {
      @tailrec
      def dropWhile_ [A] (p: A => Boolean) (acc: List[A]) (xs: List[A]) : List[A] =
        xs match {
          case Nil    => acc
          case x::xs_ => {
            val acc_ = if (p (x)) acc else x::acc
            dropWhile_ (p) (acc_) (xs_)
          }
        }

      reverse compose (dropWhile_ (p) (Nil))
    }

    def setHead [A] (h: A) : List[A] => List[A] = {
      case Nil   => Nil
      case x::xs => h::xs
    }

    
    def init [A] : List[A] => List[A] = {
      @tailrec
      def init_ [A] (acc: List[A]) (xs: List[A]) : List[A] =
        xs match {
          case Nil    => acc
          case x::Nil => acc
          case x::xs_ => init_ (x::acc) (xs_)
        }

      reverse compose (init_ (Nil))
    }

    def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
      l match {
        case Nil   => z
        case x::xs => f(x, foldRight(xs, z)(f))
    }

    // Short circuiting product with foldr --> Construct list lazily and make the accumulator lazy

    def length [A] : List[A] => Int =
      foldRight (_, 0) ((_, l) => l + 1)

    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
      l match {
        case Nil   => z
        case x::xs => foldLeft (xs, f (z, x)) (f)
      }

    def sum_ : List[Int] => Int =
      foldLeft (_,0) (_+_)

    def product_ : List[Int] => Int =
      foldLeft (_,1) (_*_)

    def length_ : List[Int] => Int =
      foldLeft (_,0) ((l,_) => l + 1) 

    def uncurry [A,B,C] : (A => B => C) => (A,B) => C =
      f => (a,b) => f (a) (b)

    def reverse_ [A] : List[A] => List[A] =
      foldLeft[A,List[A]] (_,Nil) ((xs,x) => x::xs)

    def id [A] : A => A = a => a

    def foldl [A,B] : List[A] => B => ((B,A) => B) => B =
      xs => a => f =>
        foldRight[A,B=>B] (xs,id) ((x, acc) => zacc => f (acc (zacc), x)) (a)

    /*
     * Build a function (f :: b -> b) which takes the initial accumulator.
     * Foldl needs it's initial accumulator in the first step,
     * but foldr only gives it at the last step.
     * 
     * Thus we build a big closure:
     *   foldl f z [i,j] = foldr (\..) id [i,j] z
     *                   = foldr (\..) (\z' -> f (id z') i) [j] z
     *                   = foldr (\..) (\z'' -> f ((\z' -> f (id z') i) z'') j) [] z
     *                   = (\z'' -> f ((\z' -> f (id z') i) z'') j) z
     *                   = f ((\z' -> f (id z') i) z) j
     *                   = f (f (id z) i) j
     *                   = f (f z i) j
     *
     * foldl :: (b -> a -> b) -> b -> [a] -> b
     * foldl f z xs = foldr (\x acc z' -> f (acc z') x) id xs z
     *
     * foldr :: (a -> b -> b) -> b -> [a] -> b
     * foldr f z xs = foldl (\acc x z' -> acc (f x z')) id xs z 
     */

    // Concatenate to the front of the list.
    def append [A] : List[A] => List[A] => List[A] =
      xs => ys => foldRight (xs,ys) (_::_)

    // Associates to the right and append is O(length first arg) => flatten is O(sum lengths)
    def flatten [A] : List[List[A]] => List[A] =
      foldRight[List[A], List[A]] (_, Nil) (append (_) (_))

  }
}

