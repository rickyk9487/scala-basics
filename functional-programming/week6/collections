package week6

object collections {

  val xs = Array(1, 2, 3, 44)
  xs map (x => x * 2)

  val s = "Hello World"
  s filter (c => c.isUpper)
  s exists (c => c.isUpper)
  s forall (c => c.isUpper)

  val r = "Hello Ricky"

  val t = r zip s
  // Vector((H,H), (e,e), (l,l), (l,l), (o,o), ( , ), (R,W), (i,o), (c,r), (k,l), (y,d))

  // Cartesian pairs
  val M = 4
  val N = 7
  (1 to M) flatMap (x => (5 to N) map (y => (x, y)))
  // Vector((1,5), (1,6), (1,7), (2,5), (2,6), (2,7), (3,5), (3,6), (3,7), (4,5), (4,6), (4,7))

  // Scalar product

  val u = Vector(1.0, 2.1, 3.2)
  val v = Vector(3.3, 2.2, 1.1)

  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
    (xs zip ys).map(xy => xy._1 * xy._2).sum
  // Double = 11.440000000000001

  // inefficient, but elegant prime check
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)
  isPrime(97) // Boolean = true

}