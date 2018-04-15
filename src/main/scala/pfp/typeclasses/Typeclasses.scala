package pfp.intro

/**
  * Ablility to write generic code.
  * Being more abstract and more safety.
  */

/**
  * Why do we need type class?
  * Type class is a set of Type. Haskell build within the language in Scala built using implicits
  * Being more abstract
  */
object Typeclasses {

  trait Show[A] {
    def show(a: A): String
  }

  object Show {
    def apply[A: Show]: Show[A] = implicitly[Show[A]]

    implicit class ShowOps[A : Show](a: A) {
      def show: String = Show[A].show(a)
    }
  }

  class User(val firstName: String, val lastName: String)

  //def hello[A](a: A)(implicit sh: Show[A]): String
  def hello[A : Show](a: A): String = {
    //"hi" + implicitly[Show[A]].show(a)
    //"hi" + Show.apply[A].show(a)
    //"hi" + Show[A].show(a)
    import Show.ShowOps
    "hi " + a.show
    //"hi " + new ShowOps(a).show
    //"hi " + Show[A].show(a)
  }



  def main(args: Array[String]): Unit = {
    implicit val UserShow: Show[User] = new Show[User] {
      override def show(user: User): String = user.firstName + " " + user.lastName
    }

    val user = new User("Jugal", "Shah")
    println(hello(user))
  }

}

