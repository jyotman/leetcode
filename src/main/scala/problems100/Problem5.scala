package problems100

object Problem5 {
  def longestPalindrome(s: String): String = {

    def find(s: String, index: Int, max: String = "", visited: Set[Int] = Set()): String = {
      if (visited.contains(index))
        return max
      if (index < 0 || index == s.length)
        max
      else {
        var newMax = greaterPalindrome(max, s(index).toString)
        if ((index - 1) >= 0 && (index + 1) < s.length && s(index - 1) == s(index + 1))
          newMax = greaterPalindrome(maxPalindrome(s, index - 1, index + 1), newMax)
        if ((index - 1 >= 0) && s(index) == s(index - 1))
          newMax = greaterPalindrome(maxPalindrome(s, index - 2, index + 1), newMax)
        if ((index + 1) < s.length && s(index) == s(index + 1))
          newMax = greaterPalindrome(maxPalindrome(s, index - 1, index + 2), newMax)
        greaterPalindrome(find(s, index - 1, newMax, visited + index), find(s, index + 1, newMax, visited + index))
      }
    }

    def maxPalindrome(s: String, i: Int, j: Int): String = {
      if (i >= 0 && j < s.length && s(i) == s(j))
        maxPalindrome(s, i - 1, j + 1)
      else
        s.substring(i + 1, j)
    }

    def greaterPalindrome(s1: String, s2: String): String = {
      if (s1.length > s2.length) s1
      else s2
    }

    find(s, s.length / 2)
  }
}