package pfp.tools

import scala.language.higherKinds
import scalaz._, Scalaz._, concurrent._

/**
  * Type constructor - Option is a Type constructor.
  * Functor is a HKT - Higher Kinded Type
  *
  */
object Toolbox {

  val length: String => Int = _.length

  val plus: (Int, Int) => Int = _ + _

  def lengthOperation(str: String): Int = length(str)

  def lengthOperation(str: Option[String]): Option[Int] =
    str.map(length)

  def lengthOperation(str: Throwable \/ String): Throwable \/ Int =
    str.map(length)

  def lengthOperation(str: Task[String]): Task[Int] =
    str.map(length)

  //F[_] Type constructor and String is closed over F
  //FP - beauty is inversion of control
  def lengthOperation[F[_] : Functor](str: F[String]): F[Int] =
    str.map(length)
    //Functor[F].map(str)(length)

  //We are not able to apply two arguments to functor for a function that takes two arguments!
  def plusOperation[F[_] : Apply](f1: F[Int], f2: F[Int]): F[Int] = {
    val temp: F[Int => Int] = f1.map(plus.curried)//We just applied one single argument, not able to apply second argument
    Apply[F].ap(f2)(temp)// We need another type class to rescue - Apply is already a Functor, both arugments and function closed over F

    //Same this as above. Apply in Haskell is just an operator
    f2 <*> temp

    //Same thing as above
    //Apply is a Functor with additional method ap
    Apply[F].apply2(f1, f2)(plus)

    //Applicative Builder - convenient syntax to deal with application. Not found in Haskell
    //Screen operator
    (f1 |@| f2)(plus)

    //Validation is applicative - ability to accumulate errors. Functions return value or also return errors
    //Apply just gave an ability to apply arguments to a function closed over F
  }

  //Each of type classes - you add power to your type. Sometimes you need more functionaly and you need to use more powerful type classes.
  def plusOperation[F[_] : Applicative](f1: F[Int], f2: Int): F[Int] = {
    //Applicative is an Apply with one more method pure or point - lift the value of type A to F[A]
    //Ability to lift Int to F[Int]

    //Need to implement three methods: point, ap and map
    //Functor - map
    //Apply - map, ap
    //Applicative - map, ap, point - ability to lift your values into F
    //Bind - flatMap
    //Monad is just Applicative with Bind
    //Semigroup - Type class which has append - associativity
    //Mononid - is just a Semigroup with a zero value

    (f1 |@| Applicative[F].point(f2))(plus)
    (f1 |@| f2.point[F])(plus)
  }

  def plusOperation[F[_] : Apply](seed: String, f1: F[Int], f2: String => F[Int]): F[Int] = {
    (f1 |@| f2(seed))(plus)
  }

  //Bind is called flatMap in Scala
  //for comprehensions can only use if you have a Monad
  def plusOperation[F[_] : Bind](seed: F[String], f1: F[Int], f2: String => F[Int]): F[Int] = {
    (f1 |@| Bind[F].bind(seed)(f2))(plus)
    //(f1 |@| seed.bind(f2))(plus)
    (f1 |@| (seed >>= f2))(plus)

    seed >>= (s =>
      (f1 >>= (a =>
        f2(s).map(b =>
          plus(a, b)))))

    //for comprehensions can only use if you have a Monad
    for {
      s <- seed
      a <- f1
      b <- f2(s)
    } yield plus(a, b)
  }


  //Type class that combines two type classes together - Applicative and Bind
  //Applicative - point - ability to lift our value to whatever F is
  def sum(i1: Int, i2: Int): Int = i1 - i2

  def sum[A](i1: A, i2: A): A = i1 //or i2

  //Semigroup - Type class which has append - associativity
  def sum[A: Semigroup](a1: A, a2: A): A = a1 |+| a2

  def sum(list: List[Int]): Int = list.fold(0)(_ + _)

  //Mononid - is just a Semigroup with a zero value
  def sum[A : Monoid](list: List[A]): A = list.fold(Monoid[A].zero)(_ |+| _)

  //Foldable - foldMap
  def sum[F[_] : Foldable, A : Monoid](f: F[A]): A = f.fold

  def main(args: Array[String]): Unit = {
    val str = "hello"
    val maybeStr: Option[String] = "hello".some
    val strErr: Throwable \/ String = "hello".right
    val fetchStr: Task[String] = Task.delay(str)

    type Error[A] = Throwable \/ A
    type Id[A] = A

    lengthOperation(str)
    lengthOperation[Id](str)
    lengthOperation[Option](maybeStr)
    lengthOperation[Error](strErr)
    lengthOperation[Throwable \/ ?](strErr)
    lengthOperation[Task](fetchStr)

    val i1 = 10
    val i2 = 10
    plusOperation[Option](i1.some, i2.some)
    plusOperation[Option](i1.some, i2)
    plusOperation[Option](i1.some, i2)

    //plusOperation[Option]("hello", i1.some, _.length >>> (_.some))
    plusOperation[Option]("hello", i1.some, ((s: String) => Some(s.length)))
    //Now lets day seed closed over F
    plusOperation[Option]("hello".some, i1.some, ((s: String) => Some(s.length)))
  }

}
