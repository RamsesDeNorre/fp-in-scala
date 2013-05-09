
import scala.annotation.tailrec

object Chap6 {

  object Random {

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

    type Rand[+A] = RNG => (A, RNG)

    def unit [A] : A => Rand[A] =
      a => r => (a, r)

    val int : Rand[Int] = _.nextInt

   def map [A,B] (r: Rand[A]) (f: A => B) : Rand[B] =
      rng => r (rng) lmap (f)

   // ex 5

   def positiveMax (n: Int) : Rand[Int] =
      map (positiveInt) (_ / (Int.MaxValue / n))

   // ex 6

    val doubleA : Rand[Double] =
      map (positiveInt) (_.toDouble / (Int.MaxValue.toDouble + 1))

    // ex 7

    def map2 [A,B,C] (ra: Rand[A], rb: Rand[B]) (f: (A, B) => C) : Rand[C] = rng1 => {
      val (a, rng2) = ra (rng1)
      val (b, rng3) = rb (rng2)
      (f (a, b), rng3)
    }

    // ex 8

    def sequence [A] (as: List[Rand[A]]) : Rand[List[A]] =
      as .foldRight [Rand[List[A]]] (unit (Nil)) (map2 (_,_) (_::_))

    def ints_ (count: Int) (r: RNG): (List[Int], RNG) =
      sequence (List.fill (count) (int)) (r)

    // ex 9

    def flatMap [A,B] (f: Rand[A]) (g: A => Rand[B]): Rand[B] = r => {
      val (a, r2) = f (r)
      g (a) (r2)
    }

    // ex 10

    def map_ [A,B] (r: Rand[A]) (f: A => B) : Rand[B] =
      flatMap (r) (unit compose f)

    def map2_ [A,B,C] (ra: Rand[A], rb: Rand[B]) (f: (A,B) => C) : Rand[C] =
      flatMap (ra) { a =>
        flatMap (rb) { b =>
          unit (f (a, b))
        }
      }

  }

  object State {

    import State._

    case class State [S,+A] (run: S => (A,S)) {
    
      // ex 11
    
      def map [B] (f: A => B) : State [S,B] =
        State (s => run (s) lmap (f))

      def flatMap [B] (f: A => State [S,B]) : State [S,B] =
        State { s =>
          val (a, s_) = run (s)
          f (a) run (s_)
        }

    }

    object State {
    
      // ex 11 

      def unit [S,A] (a: A) : State [S,A] =
        State (s => (a,s))

      def map2 [S,A,B,C] (sa: State [S,A], sb: State [S,B]) (f: (A, B) => C) : State [S,C] =
        for { a <- sa
              b <- sb
            } yield f (a, b)

      def sequence [S,A] (as : List [State [S,A]]) : State [S,List [A]] =
        as .foldRight [State [S,List [A]]] (unit (Nil)) (map2 (_,_) (_::_))

    }

    // ex 12

    def get [S] : State [S,S] =
      State (s => (s,s))

    def set [S] (s: S) : State [S,Unit] =
      State (_ => ((),s))
     
    def modify [S] (f: S => S) : State [S, Unit] = for {
      s <- get
      _ <- set (f (s))
    } yield ()

    def when [S] (c: Boolean) (t: State [S, Unit]) : State [S,Unit] =
      State { s =>
        if (c) t run (s)
        else ((),s)
      }

    // ex 13

    object Candy {

      sealed trait Input
      case object Coin extends Input
      case object Turn extends Input

      case class Machine (locked: Boolean, candies: Int, coins: Int)

      def empty (m: Machine) : Boolean =
        m.candies <= 0

      val insert : State [Machine,Unit] = for {
        m <- get
        _ <- set (Machine (false, m.candies, m.coins + 1))
      } yield ()

      val turn : State [Machine,Unit] = for {
        m <- get
        _ <- when (! m.locked) {
               set (Machine (true, m.candies - 1, m.coins))
             }
      } yield ()

      val coins : State [Machine,Int] = get map (_.coins)

      def step (in: Input) : State [Machine,Unit] = for {
        m <- get
        _ <- when (! empty (m)) (in match {
               case Coin => insert
               case Turn => turn
             })
        } yield ()

      def simulateMachine (in: List[Input]): State [Machine, Int] = for {
        _ <- sequence (in map (step))
        c <- coins
      } yield c

      // simulateMachine (List(Coin,Coin,Turn,Coin,Turn,Turn)) run (Machine (true, 4, 0))
      // Turn has to be preceded by Coin to have effect, Turn always locks the machine!

    }

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

