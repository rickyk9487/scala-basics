package week5

object listfun {
  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "orange", "pineapple", "banana")

  nums filter( x=> x > 0)
  // List[Int] = List(2, 5, 7, 1)

  nums filterNot (x => x > 0)
  // List[Int] = List(-4)

  nums partition (x => x > 0)
  //(List[Int], List[Int]) = (List(2, 5, 7, 1),List(-4))

  nums takeWhile (x => x > 0)
  // List[Int] = List(2)
  nums dropWhile (x => x > 0)
  // List[Int] = List(-4, 5, 7, 1)

  nums span(x => x > 0)
  // (List[Int], List[Int]) = (List(2),List(-4, 5, 7, 1))

  val data = List("a", "a", "a", "b", "c", "c", "a")

  def pack[T] (xs: List[T]): List[List[T]] = xs match {
    case Nil      => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }
  // List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a))


  def encode[T](xs: List[List[T]]): List[(T, Int)] = xs match {
    case Nil => Nil
    case x :: xs1 => (x.head, x.length) :: encode(xs1)
  }
  // List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))


  def encode1[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))
}
  // List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
