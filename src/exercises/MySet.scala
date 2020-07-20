package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  override def apply(v1: A): Boolean = contains(v1)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
  Exercise #2
  - remove an element
  - intersection with another set
  - difference with another set
   */

  def -(elem: A): MySet[A]

  def &(other: MySet[A]): MySet[A]

  def --(other: MySet[A]): MySet[A]

  /*
  Excercise #3
  Implement unary_!
   */
  def unary_! : MySet[A]

}

class EmptySet[A] extends MySet[A] {

  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def foreach(f: A => Unit): Unit = ()

  override def apply(v1: A): Boolean = super.apply(v1)

  override def -(elem: A): MySet[A] = this

  override def &(other: MySet[A]): MySet[A] = this

  override def --(other: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = ???
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {

  override def contains(elem: A): Boolean = {
    head == elem || tail.contains(elem)
  }

  override def +(elem: A): MySet[A] = {
    if(this.contains(elem)) {
      this
    }
    else {
      new NonEmptySet(elem, this)
    }
  }

  override def ++(anotherSet: MySet[A]): MySet[A] = {
    tail ++ anotherSet + head
  }

  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {

    val filteredTail = tail.filter(predicate)

    if(predicate(head)) {
      filteredTail + head
    }
    else {
      filteredTail
    }
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def -(elem: A): MySet[A] = {
    if(elem == head) tail
    else tail - elem + head
  }

  override def &(other: MySet[A]): MySet[A] = this.filter(other)

  override def --(other: MySet[A]): MySet[A] = this.filter(a => !other(a))

  override def unary_! : MySet[A] = ???
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] = {
      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)
    }

    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {

  val s = MySet(1,2,3,4)

  s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, x * 10 )) filter(_ % 2 == 0) foreach println

}
