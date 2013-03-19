
import scala.annotation.tailrec

object Chap3 {

  sealed trait List[+A] {
  
    def :: [B >: A] : B => List[B] = new ::(_, this)
    
    override def toString: String = {
      
      def toString_ : List[A] => String = {
        case Nil   => "Nil"
        case x::xs => x.toString + " :: " + toString_ (xs)
      }

      "(" + toString_ (this) + ")"
    }

  }

  case object Nil extends List[Nothing]
  
  case class ::[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def nil: List[Nothing] = Nil

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
      case x::xs if p(x) => dropWhile (p) (xs)
      case l             => l
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

    @tailrec
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
      foldRight[List[A], List[A]] (_, Nil) (uncurry (append))

    def plus1 : List[Int] => List[Int] =
      foldRight[Int, List[Int]] (_,Nil) ((x,ys) => x+1 :: ys)

    def doubles2Strings : List[Double] => List[String] =
      foldRight[Double,List[String]] (_,Nil) ((x,ys) => x.toString :: ys)

    def map [A,B] (f: A => B) : List[A] => List[B] =
      foldRight[A,List[B]] (_,Nil) ((x,ys) => f(x) :: ys)

    def filter [A] (p: A => Boolean) : List[A] => List[A] =
      foldRight[A,List[A]] (_,Nil) ((x,ys) => if (p(x)) x::ys else ys)

    def flatMap [A,B] (f: A => List[B]) : List[A] => List[B] =
      flatten compose (map (f))

    def filter_ [A] (p: A => Boolean) : List[A] => List[A] =
      flatMap (x => if (p(x)) x::Nil else Nil)

    def zipWith [A,B,C] (f: A => B => C) : List[A] => List[B] => List [C] = {

      def zipWith_ (f: A => B => C) : (List[A], List[B]) => List[C] = {
        case (x::xs,y::ys) => f (x) (y) :: zipWith_ (f) (xs,ys)
        case _             => Nil
      }

      curry (zipWith_ (f))
    }


     /*******************/
    // hasSubSequences //

    def tails [A] : List[A] => List[List[A]] = {
      case Nil     => Nil::Nil
      case l@_::xs => l :: tails (xs)
    }

    def isPrefixOf [A] : List[A] => List[A] => Boolean = {
      @tailrec
      def isPrefixOf_ [A] (pre: List[A], l: List[A]) : Boolean = (pre,l) match {
        case (Nil,_)                 => true
        case (x::xs,y::ys) if x == y => isPrefixOf_ (xs,ys)
        case _                       => false
      }

      curry (isPrefixOf_)
    }

    def any [A] (p: A => Boolean): List[A] => Boolean =
      foldRight (_,false) (p(_) || _)

    def hasSubSequence [A] (sub: List[A]) : List[A] => Boolean =
      (any (isPrefixOf (sub))) compose (tails)

  }

  sealed trait Tree[+A]
  case class Leaf[+A](value: A) extends Tree[A]
  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size [A] : Tree[A] => Int = {
      case Leaf   (_)   => 0
      case Branch (l,r) => 1 + size (l) + size (r)
    }

    def maximum : Tree[Int] => Int = {
      case Leaf   (l)   => l
      case Branch (l,r) => maximum (l) max maximum (r)
    }

    def depth [A] : Tree[A] => Int = {
      case Leaf   (_)   => 1
      case Branch (l,r) => 1 + (depth (l) max depth (r))
    }

    def map [A,B] (f: A => B) : Tree[A] => Tree[B] = {
      case Leaf   (l)   => Leaf (f(l))
      case Branch (l,r) => Branch (map (f) (l), map (f) (r))
    }

    def fold [A,B] (f: A => B) (h: B => B => B) : Tree[A] => B = {
      case Leaf   (l)   => f (l)
      case Branch (l,r) => h (fold (f) (h) (l)) (fold (f) (h) (r))
    }

    def fold_ [A,B] (f: A => B) (h: (B,B) => B) : Tree[A] => B =
      fold (f) (h.curried)

    def const [A,B] : A => B => A = a => _ => a

    def id [A] : A => A = a => a

    def size_ [A] : Tree[A] => Int =
      fold_ (const (0)) (_ + _ + 1)

    def maximum_ : Tree[Int] => Int =
      fold_[Int,Int] (id) (_ max _)

    def depth_ [A] : Tree[A] => Int =
      fold_ (const (1)) ((x,y) => (x max y) + 1)

    def map_ [A,B] (f: A => B) : Tree[A] => Tree[B] =
      fold_[A,Tree[B]] ((Leaf (_:B)) compose f) (Branch (_, _))

  }

  import scala.language.higherKinds

  trait Functor [F[_]] {
    def map [A,B] : (A => B) => F[A] => F[B]
  }

  object Functor {
    def map [F[_]: Functor, A, B] (g: A => B) (fa: F[A]) : F[B] =
      implicitly[Functor[F]].map (g) (fa)
  }

  implicit object ListFunctor extends Functor[List] {
    override def map [A,B] : (A => B) => List[A] => List[B] =
      List.map (_)
  }

  implicit object TreeFunctor extends Functor[Tree] {
    override def map [A,B] : (A => B) => Tree[A] => Tree[B] =
      Tree.map (_)
  }

  import Functor.map
  import List._

  val succ  = (_:Int)+1
  val test1 = map (succ) (1::2::3::nil: List[Int])
  val test2 = map (succ) (nil: List[Int])
  val test3 = map (succ) (Leaf(3): Tree[Int])

}

