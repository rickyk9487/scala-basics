package week5

abstract class List[T] {
  def foldRight[U](z: U)(op: (T, U) => U): U = this match {
    case Nil => z
    case x :: xs => op(x, (xs foldRight z)(op))
  }

  def foldLeft[U](z: U) (op: (U, T) => U): U = this match {
    case Nil => z
    case x :: xs => (xs foldLeft op(z, x))(op)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_::_) // works

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldLeft ys)(_::_) // fails
}
