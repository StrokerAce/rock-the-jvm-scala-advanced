package lectures.part1as

import scala.util.Try

object DarkSugars extends App {

  // syntax sugar #1: methods with single parameter
  def singleArgMethod(arg: Int): String = s"$arg little ducks..."

  val description = singleArgMethod{
    42
  }

  val aTryInstance = Try { // java's try
    throw new RuntimeException
  }

  List(1,2,3).map{
    x => x + 1
  }

  // syntax sugar #2: single abstract method
  trait Action{
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x: Int) => x + 1 // Single abstract method

  // example: Runnables

  val aThread = new Thread(
    new Runnable{
      override def run(): Unit = println("hello, scala")
    }
  )

  val aSweeterThread = new Thread(() => println("sweet, scala"))

  // Also works for abstarct types with a single unimplemented method

  // syntax sugar #3: :: and #: are special

  val prependedList = 2 :: List(3, 4)
  // 2.::List(3, 4)
  // List(3, 4).::(2)
  // Last char determines associativity of of method

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  // Syntax sugar #4: Multi word method naming

  class TeenGirl(name: String) {
    def `and then said`(gossip: String) = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")

  lilly `and then said` "Scala is so sweet"

  // syntax sugar #5: infix types
  class Composite[A, B]

  val composite: Int Composite String = new Composite[Int, String]

  // syntax sugar #6: update() is very special, much like apply

  val anArray = Array(1, 2, 3)
  anArray(2) = 7 // rewritten to anArray.update(2,7) - Used in mutable collections

  // syntax sugar #7: setters for mutable containers

  class Mutable {
    private var internalMember: Int = 0
    def member = internalMember
    def member_=(value: Int): Unit = internalMember = value
  }

  val aMutableContainer = new Mutable

  aMutableContainer.member = 42
}
