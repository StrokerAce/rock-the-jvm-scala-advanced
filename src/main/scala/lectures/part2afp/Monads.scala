package lectures.part2afp

object Monads extends App {

  trait Attempt[+A]{
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt{
    def apply[A](a: => A): Attempt[A] = {
      try{
        Success(a)
      }
      catch {
        case e: Throwable => Fail(e)
      }
    }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] = {
      try{
        f(value)
      }
      catch{
        case e: Throwable => Fail(e)
      }
    }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  val attempt = Attempt{
    throw new RuntimeException("My own monad, yes!")
  }

  println(attempt)

  /*
  Excercise:
  1) implement a Lazy[T] monad = computation that will only be executed when it's needed
     unit/apply
     flatMap
  2) Monads = unit + flatmap
     Monads = unit + map + flatten

     def Monad[T] {
       def flatMap[B](f: T => Monad[B]): Monad[B] = // Implemented

       def map[B](f: T => B): Monad[B] = ???
       def flatten(m: Monad[Monad[T]]): Monad[T] = ???

     }

   */

  class Lazy[+A](value: => A) {
    // Call by need
    private lazy val internalValue = value
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)
    def use: A = value
  }

  object Lazy{
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy{
    println("Today I don't feel like doing anything!")
    42
  }

  val flatMapped = lazyInstance.flatMap(x => Lazy{
    println("Dude")
    10 * x
  })

  val flatMapped2 = lazyInstance.flatMap(x => Lazy{
    println("Dude")
    10 * x
  })

  flatMapped.use
  flatMapped2.use

  //: Map and flatten in terms of flatMap

  // map[B](f: T => B): Monad[B] = flatMap(x => unit(f(x)))
  // List(1,2,3).map(_ * 2) = List(1, 2, 3).flatMap(x => List(x * 2)) = List(2,4,6)

  // flatten(m: Monad[Monad[Y]]): Monad[T] = m.flatMap((x: Monad[T]) => x)
  // List(List(1,2), List(3,4)) = flatten = List(List(1,2), List(3,4)).flatMap(x => x) = List(1,2,3,4)

}
