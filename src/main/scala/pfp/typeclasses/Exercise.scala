package pfp.typeclasses

object Exercise {
 
  /** Typeclasses */
  // 1. Implement Equalz[A] typeclass that has method def eq(a1: A, a2: A): Boolean

  trait Equalz[A] {
    def eq1(a1: A, a2: A): Boolean
  }

  object Equalz {
    def apply[A: Equalz]: Equalz[A] = implicitly[Equalz[A]]

    implicit class EqualzOps[A : Equalz](a: A) {
      def eq1(a2: A): Boolean = Equalz[A].eq1(a, a2)
    }
  }

  implicit val StringEqz: Equalz[String] = new Equalz[String] {
    import Equalz._
    override def eq1(a1: String, a2: String): Boolean = a1 == a2
  }

  implicit def TupleEqz[A : Equalz, B : Equalz]: Equalz[(A, B)] = new Equalz[(A, B)] {
    import Equalz._
    override def eq1(a1: (A, B), a2: (A, B)): Boolean = a1._1.eq1(a2._1) && a1._2.eq1(a2._2)
  }

  // 2. Write EqualzOps & Equalz companion object with apply method

  // 3. Write instance for String and User
  case class User(login: String)

  def main(args: Array[String]): Unit = {

    implicit val UserEqz: Equalz[User] = new Equalz[User] {
      import Equalz._
      override def eq1(a1: User, a2: User): Boolean = a1.login.eq1(a2.login)
      //override def eq1(a1: User, a2: User): Boolean = a1.login == a2.login
    }

  }

  // 4. if A & B have instance for Equalz, can u write generic instance for Tuple (A, B)
}
