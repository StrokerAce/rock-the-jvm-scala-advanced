package lectures.part1as

object AdvancedPatternMatching extends App {

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"The only element is $head")
    case _ =>
  }

  /*
  - constants
  - wildcards
  - case classes
  - some special magic like above
   */

  class Person(val name: String, val age: Int)

  object Person{
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None else Some((person.name, person.age))

    def unapply(age:Int): Option[String] = Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a years old"
  }

  println(greeting)

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status"
  }

  println(legalStatus)

  /*
  Exercise.
   */

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  val mathProperty = 8 match {
    case even() => "An even number"
  }

  println(mathProperty)

  // infix patterns
  case class Or[A, B](a: A, b: B)

  val either = Or(2, "two")

  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }

  println(humanDescription)

  // decomposing sequences

  val vararg = numbers match {
    case List(1, _*) => "Starting with 1"
  }

  println(vararg)

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]):Option[Seq[A]] = {
      if(list == Empty) Some(Seq.empty[A])
      else unapplySeq(list.tail).map(list.head +: _)
    }
  }

  val myList = Cons(1, Cons(2, Cons(3, Empty)))

  val decomposed = myList match {
    case MyList(1, 2, _*) => "Starting with 1, 2"
    case _ => "Something else"
  }

  println(decomposed)

  // Custom return types for unapply
  // isEmpty: Boolean, get: something

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String]{
      def isEmpty = false
      def get = person.name
    }
  }

  println(
    bob match{
      case PersonWrapper(n) => s"This persons name is $n"
      case _ => "An alien"
    }
  )

  println(
    Option(1) match {
      case PersonWrapper(n) => "Bogus"
      case Some(n) => s"This option contains $n"
    }
  )



}
