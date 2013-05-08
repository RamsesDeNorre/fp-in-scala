
import scala.annotation.tailrec

object Chap6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) &
                    ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
      }
    }
  }

  // ex 1

  def positiveInt (rng: RNG) : (Int, RNG) =
    until [(Int,RNG)] (_._1 >= 0) (rng.nextInt) (_._2.nextInt lmap (_.abs))

  // ex 2

  def double (rng: RNG) : (Double, RNG) =
    positiveInt (rng) lmap (_.toDouble / (Int.MaxValue.toDouble + 1))

  // ex 3

  def intDouble (r0: RNG): ((Int,Double), RNG) = {
    val (i, r1) = r0.nextInt
    val (d, r2) = double (r1)
    ((i, d), r2)
  }

  def doubleInt(r: RNG): ((Double,Int), RNG) =
    intDouble (r) lmap (_.swap)

  def double3(r0: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double (r0)
    val (d2, r2) = double (r1)
    val (d3, r3) = double (r2)
    ((d1, d2, d3), r3)
  }

  // ex 4

  def ints (count: Int) (r: RNG): (List[Int], RNG) =
    until [(List[Int], RNG)] (_._1.length == count) ((Nil, r)) { p =>
      val (i, r2) = p._2.nextInt
      (i :: p._1, r2)
    }

  // utilities

  @tailrec
  def until [A] (f: A => Boolean) (a: A) (g: A => A) : A =
    if (f (a)) a
    else until (f) (g (a)) (g)

  implicit class PairFunctor [A,B] (p: (A,B)) {
    def lmap [C] (f: A => C) : (C,B) =
      (f(p._1), p._2)
  }

}

