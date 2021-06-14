package module1


object opt {
  sealed trait MyOption[+A]{
    def isEmpty: Boolean = this match {
      case MyOption.Some(_) => false
      case MyOption.None => true
    }

    def get: A = this match {
      case MyOption.Some(v) => v
      case MyOption.None => throw new Exception("Get on empty list")
    }


    def getOrElse[B >: A](b: B): B = this match {
      case MyOption.Some(v) => v
      case MyOption.None => b
    }

    def map[B](f: A => B): MyOption[B] = this match {
      case MyOption.Some(v) => MyOption.Some(f(v))
      case MyOption.None => MyOption.None
    }

    def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
      case MyOption.Some(v) => f(v) match {
        case MyOption.Some(vv) => MyOption.Some(vv)
        case MyOption.None => MyOption.None
      }
      case MyOption.None => MyOption.None
    }


    def f(x: Int, y: Int): MyOption[Int] =
      if(y == 0) MyOption.None
      else MyOption.Some(x / y)

  }

  object MyOption{
    case class Some[A](v: A) extends MyOption[A]
    case object None extends MyOption[Nothing]
  }
}
