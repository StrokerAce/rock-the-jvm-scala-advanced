package lectures.typeclass

object Typeclass extends App {
  // problem
  // specialized implementations

  // implicits
  trait Summable[T] {
    def sumElements(list: List[T]): T
  }

  implicit object IntSummable extends Summable[Int] {
    override def sumElements(list: List[Int]) = list.sum
  }

  implicit object StringSummable extends Summable[String] {
    override def sumElements (list: List[String]) = list.mkString("")
  }

  def processMyList[T](list: List[T])(implicit summable: Summable[T]): T = { // ad-hoc polymorphism
    // "sum up" all the elements in the list
    // Int  = sum, String = concat
    // other types = Error
    summable.sumElements(list)
  }

  val intSum = processMyList(List(1,2,3))
  val stringSum = processMyList(List("Scala ", "is ", "awesome"))

  println(intSum)
  println(stringSum)
}
