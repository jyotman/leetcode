package problems100

object Problem8 {
  def myAtoi(str: String): Int = {
    val withoutWhitespaces = str.dropWhile(c => c == ' ')
    val (numbers, sign) = {
      if (withoutWhitespaces.nonEmpty && (withoutWhitespaces.head == '+' || withoutWhitespaces.head == '-'))
        (withoutWhitespaces.tail, withoutWhitespaces.head)
      else
        (withoutWhitespaces, '+')
    }
    if (numbers.length == 0)
      0
    else {
      val number = getFirstNumber(numbers)
      try {
        if (number == "")
          0
        else if (sign == '-')
          number.toInt * -1
        else
          number.toInt
      } catch {
        case e: NumberFormatException => {
          if (sign == '-')
            Int.MinValue
          else
            Int.MaxValue
        }
      }
    }
  }

  def getFirstNumber(s: String, index: Int = 0, result: String = ""): String = {
    if (index == s.length)
      result
    else if (s(index).isDigit)
      getFirstNumber(s, index + 1, result + s(index))
    else
      result
  }
}