def balance(chars: List[Char]): Boolean = {
  def balanced(chars: List[Char], open: Int): Boolean =
    if (chars.isEmpty) open == 0
    else if (chars.head == '(') balanced(chars.tail, open + 1)
    else if (chars.head == ')') open > 0 && balanced(chars.tail, open - 1)
    else balanced(chars.tail, open)
  balanced(chars, 0)
}

val x1 = "(just an) example".toList
val x2 = "())(".toList
val x3 = "(if (zero? x) max (/ 1 x))".toList
val x4 = "(that it’s not (yet) done). (But he wasn’t listening)".toList

balance(x1) // true
balance(x2) // false
balance(x3) // true
balance(x4) // true
