/**
 Lists, covariant (+) vs. contravariant (-) in functions
 */

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.head")
}

// List[Nothing] is a subtype of List[String] because Nothing is a subtype of String
object test{
  val x: List[String] = Nil
}

test
