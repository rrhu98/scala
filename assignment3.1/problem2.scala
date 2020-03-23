object problem2 {
  case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]

  case class Success[+B](get: B) extends Partial[Nothing,B]

  trait Partial[+A,+B] {
    def map[C] (f: B=>C):Partial[A,C] = this match {
      case Success(a)=>Success(f(a))
      case Errors(e) => Errors(e)
    }

    def flatMap[AA >: A, C](f: B=>Partial[AA,C]): Partial[AA,C] = this match {
      case Success(a)=>f(a)
      case Errors(e)=>Errors(e)
    }

    def getOrElse[BB >: B](or: => BB): BB = this match {
      case Success(b)=>b
      case _ => or
    }

    def orElse[AA >: A,BB >: B](b: => Partial[AA,BB]): Partial[AA,BB] = this match {
      case Success(a) => Success(a)
      case Errors(_) => b
    }

    def map2[AA>: A, C, D](b: Partial[AA,C])(f: (B,C)=>D): Partial[AA,D] = (this,b) match {
      case (Errors(e1),Errors(e2))=> Errors(e1 ++ e2)
      case (Errors(e), Success(_))=> Errors(e)
      case (Success(_), Errors(e))=> Errors(e)
      case (Success(a1), Success(a2))=> Success(f(a1,a2))
    }

    Object Partial {
      def traverse[E,A,B](es:List[A])(f:A => Partial[E,B]): Partial[E,List[B]] = es match {

      }
    }
  }


}
