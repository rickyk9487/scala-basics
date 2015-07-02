/* For a given positive integer n, generate pairs (i, j) such that
 * 1. 1 <= j < i < n
 * 2. i + j is prime
 */
def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)

def pairs(n: Int): IndexedSeq[IndexedSeq[(Int, Int)]] =
  (1 until n) map (i => (1 until i) map (j => (i, j)))

val n = 7

val xss = pairs(n).flatten
// Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2)), Vector((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector((6,1), (6,2), (6,3), (6,4), (6,5)))

xss filter (pair => isPrime(pair._1 + pair._2))
// IndexedSeq[(Int, Int)] = Vector((2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5))

def pairs2(n: Int): IndexedSeq[(Int, Int)] = {
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)
}

val yss = pairs2(n)
// IndexedSeq[(Int, Int)] = Vector((2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5))

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (for { (x, y) <- xs zip ys} yield x * y).sum

val u = Vector(1.0, 2.1, 3.2)
val v = Vector(3.3, 2.2, 1.1)

scalarProduct(u, v)
// Double = 11.440000000000001