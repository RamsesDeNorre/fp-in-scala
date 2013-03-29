
object Chap4 {

  sealed trait Option[+A] {
    
    def map [B] (f: A => B): Option[B] = this match {
      case Some (a) => Some (f (a))
      case _        => None
    }

    def flatMap [B] (f: A => Option[B]) : Option[B] =
      map (f) getOrElse None

    def getOrElse [B >: A] (b: B) : B = this match {
      case Some (a) => a
      case _        => b
    }

    def orElse [B >: A] (b: Option[B]) : Option[B] =
      map (Some (_)) getOrElse b

    def filter (f: A => Boolean) : Option[A] =
      flatMap (a => if (f(a)) Some (a) else None)

  }

  case class  Some[+A] (get: A) extends Option[A]

  case object None              extends Option[Nothing]


  object Option {

    def some [A] : A => Option[A] = Some (_)

    def mean (xs: Seq[Double]) : Option[Double] =
      if (xs.isEmpty) None
      else Some (xs.sum / xs.length)

    // ex 2

    def variance (xs: Seq[Double]) : Option[Double] =
      mean (xs) flatMap (m =>
        mean (xs map (x =>
          math.pow(x - m, 2))))

    def variance_ (xs: Seq[Double]) : Option[Double] =
      for {
        m <- mean (xs)
        v <- mean (xs map (x => math.pow (x - m, 2)))
      } yield v


    // ex 3

    def map2 [A,B,C] (a: Option[A], b: Option[B]) (f: (A,B) => C) : Option[C] =
      for {
        x <- a
        y <- b
      } yield f (x,y)

    def mkMatcher (pat: String) : Option[String => Boolean] =
      Some ((_: String) == "Hello")

    // ex 4

    def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
      map2 (mkMatcher(pat), mkMatcher(pat2)) ((f, g) => f(s) && g(s))

    // ex 5
    // map2 the two options using cons

    def sequence [A] (xs: List[Option[A]]) : Option[List[A]] =
      xs.foldRight (some(List():List[A])) (map2 (_,_) (_::_))

    // ex 6

    def traverse [A,B] (xs: List[A]) (f: A => Option[B]) : Option[List[B]] =
      xs.foldRight (some(List():List[B])) ((a, b) => map2 (f(a),b) (_::_))

    def sequence_ [A] (xs: List[Option[A]]) : Option[List[A]] =
      traverse (xs) (_ orElse None)

  }

  // ex 7

  sealed trait Either[+E, +A] {
    def map [B] (f: A => B): Either[E, B] = this match {
      case Right (a) => Right (f(a))
      case Left  (e) => Left  (e)
    }

    def flatMap [F >: E, B] (f: A => Either[F, B]): Either[F, B] = map (f) match {
      case Right (r) => r
      case Left  (e) => Left (e)
    }

    def orElse [F >: E,B >: A] (b: => Either[F, B]): Either[F, B] = this match {
      case Left (_) => b
      case r        => r
    }

    def map2 [F >: E, B, C] (b: Either[F, B]) (f: (A, B) => C): Either[F, C] =
      for {
        x <- this
        y <- b
      } yield f(x, y)

  }

  case class Left [+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {

    def right [E,A] : A => Either[E,A] = Right (_)

    // ex 8

    def traverse [A,B,E] (xs: List[A]) (f: A => Either[E,B]) : Either[E,List[B]] =
      xs.foldRight (right[E,List[B]](List())) ((a, b) => f(a).map2 (b) (_::_))

    def sequence_ [A,E] (xs: List[Either[E,A]]) : Either[E,List[A]] =
      traverse (xs) {
        case Left (e) => Left(e)
        case r        => r
      }
  
  }

  // ex 9

  trait Monoid [M] {
    def zero : M
    def <>   : M => M => M
  }

  object Monoid {
    def zero [M : Monoid] : M =
      implicitly[Monoid[M]].zero

    def <>   [M : Monoid] : M => M => M =
      implicitly[Monoid[M]].<>
  }

  implicit def ListMonoid[A] : Monoid[List[A]] = new Monoid[List[A]] {
    def zero : List[A] = List()
    def <>   : List[A] => List[A] => List[A] =
      ((_:List[A]) ++ (_:List[A])).curried
  }

  object MonoidalEither {

    def map [A,B,E] (f: A => B) : Either[E,A] => Either[E, B] = {
      case Right (a) => Right (f(a))
      case Left  (e) => Left  (e)
    }

    def flatMap [A, E, F >: E, B >: A] (f: A => Either[F, B]) (a: Either[E,A]): Either[F, B] = a map (f) match {
      case Right (r) => r
      case Left  (e) => Left (e)
    }

    def orElse [A, E, F >: E : Monoid,B >: A]
        (b: => Either[F, B]): Either[E,A] => Either[F, B] = {
      case Left (e) => {
        val m = implicitly[Monoid[F]]
        b match {
          case Left (e2) => Left (m.<> (e) (e2))
          case _         => Left (m.<> (e) (m.zero))
        }
      }
      case r        => r
    }

    def map2 [A, E, F >: E, B >: A, C] (a: Either[E,A]) (b: Either[F, B]) (f: (A, B) => C): Either[F, C] =
      for {
        x <- a
        y <- b
      } yield f(x, y)

  }  

}

