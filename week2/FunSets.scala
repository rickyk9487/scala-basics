/**
 * Makes the type Set on the integers in [-1000, 1000]
 */

object FunSets {
  type Set = Int => Boolean
  def contains(s: Set, elem: Int): Boolean = s(elem)
  def singletonSet(elem: Int): Set = {elem2: Int => elem2 == elem}
  def union(s: Set, t: Set): Set = {
    elem: Int => contains(s, elem) || contains(t, elem)
  }
  def intersect(s: Set, t: Set): Set = {
    elem: Int => contains(s, elem) && contains(t, elem)
  }
  def diff(s: Set, t: Set): Set = {
    elem: Int => contains(s, elem) && !contains(s, elem)
  }

  def filter(s: Set, p: Int => Boolean): Set = {
    elem: Int => contains(s, elem) && contains(p, elem)
  }

  val bound = 1000

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a == -bound) true
      else if (contains(s, a) && (!contains(filter(s, p), a))) false
      else iter(a - 1)
    }
    iter(bound)
  }

  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a == -bound) false
      else if (contains(s, a) && (!contains(filter(s, p), a))) true
      else iter(a - 1)
    }
    iter(bound)
  }

  def map(s: Set, f: Int => Int): Set = {
    elem: Int => exists(s, {elem2: Int => f(elem2) == elem})
  }

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def printSet(s: Set) {
    println(toString(s))
  }
}
