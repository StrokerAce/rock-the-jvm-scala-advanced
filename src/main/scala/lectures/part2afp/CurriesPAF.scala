package lectures.part2afp

import javax.xml.transform.OutputKeys

object CurriesPAF extends App {

  val superAdder: Int => Int => Int = x => y => x + y

  val add3 = superAdder(3)

  println(add3(5))

  println(superAdder(3)(5)) // curried function

  // Method! Not an instance of a function (JVM limitation)
  def curriedAdder(x: Int)(y: Int): Int = x + y //Curried method

  val add4: Int => Int = curriedAdder(4)

  // lifting = ETA Expansion

  def inc(x: Int) = x + 1

  List(1,2,3).map(inc) // ETA Expansion - compiler converts to List(1,2,3).map(x => inc(x))

  // Partial function application.
  val add5 = curriedAdder(5) _ // Tells compiler to perform ETA Expansion to Int => Int

  //Exercise
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  val add7 = (x: Int) => simpleAddFunction(7, x)
  val add7_2 = simpleAddFunction.curried(7)
  val add7_6 = simpleAddFunction(7, _: Int)

  val add7_3 = curriedAddMethod(7) _ // PAF
  val add7_4 = curriedAddMethod(7)(_) // PAF

  val add7_5 = simpleAddMethod(7, _: Int) // y => simpleAddMethod(7, y)

  //underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c

  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?") // x => concatenator("Hello, I'm ", x, ", how are you?")
  println(insertName("Matt"))

  val fillInTheBlanks = concatenator("Hello", _: String, _: String)

  // EXERCISES
  /*
  1. Process a list of numbers with different formats using a curried function,
  2. Difference between
     - functions v methods
     - parameters: by-name vs 0 lambda
   */

  val data = List(math.E, math.Pi)

  val formatter = (format: String, value: Any) => format.format(value)

  val fourTwoFormatter = formatter.curried("%4.2f")
  val bigFormatter = formatter.curried("%14.12f")

  println(data.map(fourTwoFormatter))
  println(data.map(bigFormatter))

  def byName(n: => Int): Int = n + 1
  def byFunction(f: () => Int): Int = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  byName(23) // OK
  byName(method) // OK
  byName(parenMethod()) // OK
  byName(parenMethod) // OK but beware  parenMethod is called ==> equivalent to byName(parenMethod())
  // byName(() => 42) not OK
  byName((() => 42)()) // OK
  //byName(parenMethod _ ) // not OK

  //byFunction(42) // not OK
  //byFunction(method) // not OK - compiler does not do eta expansion
  byFunction(parenMethod) // OK compiler does eta expansion
  byFunction(() => 46) // OK
  byFunction(parenMethod _) // also works but compiler warning - unnecessary

}
