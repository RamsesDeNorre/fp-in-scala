
object Chap3 {

  sealed trait Tree[+A]
  case class Leaf  [+A](value: A)                      extends Tree[A]
  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size [A] (t: Tree[A]) : Int = {

      def size_ (t: Tree[A]) (k: Int => Int) : Int = t match {
        case Leaf   (_)   => k (0)
        case Branch (l,r) => size_ (l) (i => size_ (r) (j => k (1 + i + j)))
      }

      size_ (t) (identity)
    }

    def fold [A,B] (t: Tree[A]) (f: A => B) (h: B => B => B) : B = {
      
      def fold_ (t: Tree[A]) (k: B => B) : B = t match {
        case Leaf   (l)   => (k compose f) (l)
        case Branch (l,r) => fold_ (l) (bl =>
                               fold_ (r) (br =>
                                 k (h (bl) (br))))
      }

      fold_ (t) (identity)
    }

  }

}

