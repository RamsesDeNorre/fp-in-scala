import scala.annotation.tailrec

object MyModule {

  case class Box(height: Double, width: Double)

  def greaterBy(x: Box, y: Box, f: Box => Double) =
    if (f(x) > f(y)) x else y

  def taller : (Box, Box) => Box = greaterBy(_, _, _.height)

  def wider  : (Box, Box) => Box = greaterBy(_, _, _.width)


  def absolute(f: Int => Int) : Int => Int =
    ((_:Int).abs) compose f

  def polyAbsolute [A] : (A => Int) => A => Int =
    // cannot call this infix :(
    compose ((_:Int).abs) (_)



  type Pred [A] = A => Boolean

  def divisibleBy(k: Int) : Pred[Int] = _ % k == 0

  def even : Pred[Int] = divisibleBy(2)


  def lift [A,B,C,D] : (B => C => D) => (A => B) => (A => C) => A => D =
    f => g1 => g2 => a => f (g1(a)) (g2(a))

  def curry [A,B,C] : ((A,B) => C) => A => B => C =
    f => a => b => f(a,b)

  def uncurry [A,B,C] : (A => B => C) => (A,B) => C =
    f => (a,b) => f (a) (b)

  def div3and5 : Pred[Int] = lift (&&&) (divisibleBy(3)) (divisibleBy(5))
  
  def div3or5  : Pred[Int] = lift (|||) (divisibleBy(3)) (divisibleBy(5))

  def &&& : Boolean => Boolean => Boolean = curry (_&&_)

  def ||| : Boolean => Boolean => Boolean = curry (_||_)


  def compose [A,B,C] : (B => C) => (A => B) => A => C =
    h => g => a => h(g(a))


  def lift3 [A,B,C,D,E] : (B => C => D => E) =>
                          (A => B) =>
                          (A => C) =>
                          (A => D) =>
                           A => E =
    f => g1 => g2 =>
      lift[A,A,D,E] (lift (f) (g1) (g2)) (id)

  def id [A] : A => A = a => a


  def fib : Int => Int = {
    n =>
      
      @tailrec
      def fib_acc(n: Int, p2: Int, p1: Int) : Int =
        if (n == 0) p1 else fib_acc (n - 1, p1, p1 + p2)

      fib_acc (n, 1, 0)
  }

  @tailrec
  def iterateWhile [A] (a: A)(f: A => A, p: Pred[A]): A =
    p(a) match {
      case true  => iterateWhile (f(a)) (f, p)
      case false => a
    }

}

