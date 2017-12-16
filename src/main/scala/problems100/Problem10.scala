package problems100

import scala.collection.mutable.HashMap

object Problem10 {

  var memory: HashMap[(String, String), Boolean] = new HashMap()

  def isMatch(s: String, p: String): Boolean = {
    var res = false
    if (s.length == 0 && p.length == 0)
      res = true
    else if (s.isEmpty && !p.tail.isEmpty && p.tail.head == '*')
      res = isMatch(s, p.tail.tail)
    else if (s.isEmpty || p.isEmpty)
      res = false
    else if (memory.contains((s, p)))
      res = memory((s, p))
    else if (p.head == '.' || p.head == s.head) {
      if (!p.tail.isEmpty && p.tail.head == '*')
        res = isMatch(s.tail, p) || isMatch(s.tail, p.tail.tail) || isMatch(s, p.tail.tail)
      else
        res = isMatch(s.tail, p.tail)
    }
    else if (!p.tail.isEmpty && p.tail.head == '*' && !p.tail.tail.isEmpty)
      res = isMatch(s, p.tail.tail)
    if (!memory.contains((s, p)))
      memory += ((s, p) -> res)
    memory((s, p))
  }
}