package problems100

import scala.collection.immutable.HashMap

object Problem3 {
  def lengthOfLongestSubstring(s: String): Int = {

    def getLength(s: Array[Char], index: Int = 0, used: HashMap[Char, Int] = HashMap(), counter: Int = 0, max: Int = 0): Int = {
      if (index == s.length)
        max
      else if (used.contains(s(index)))
        getLength(s, used(s(index)) + 1, max = max)
      else
        getLength(s, index + 1, used + (s(index) -> index), counter + 1, Math.max(counter + 1, max))
    }

    getLength(s.toCharArray)
  }
}