package problems100

object Problem9 {
  def isPalindrome(x: Int): Boolean = {
    if (x < 0 || x % 10 == 0 && x != 0)
      false
    else {
      var revertedNumber = 0
      var originalNumber = x
      while (revertedNumber < originalNumber) {
        revertedNumber = revertedNumber * 10 + originalNumber % 10
        originalNumber = originalNumber / 10
      }

      revertedNumber == originalNumber || revertedNumber / 10 == originalNumber
    }
  }
}
