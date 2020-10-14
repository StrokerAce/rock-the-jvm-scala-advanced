package lectures.refined

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._ // implicits
import eu.timepit.refined.numeric._

object RefinedTypes extends App {

  case class User(name: String, email: String)

  val daniel = User("daniel", "daniel@rockthejvm.com")
  val invalidDaniel = User("daniel@rockthejvm.com", "Daniel")

  /*
  - validate logic at creation time
  - use value types
  - smart constructors
   */

  // only positive number
  val aPositiveInterview: Refined[Int, Positive] = 42 // macros + implicit conversions
  //val anInvalidInteger: Refined[Int, Positve] = -100


  println()
}
