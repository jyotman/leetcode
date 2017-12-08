package problems100

object Problem7 {

  def reverse(x: Int): Int = {
    try {
      val ans = x.toString.dropWhile(char => char == '-').reverse.dropWhile(char => char == '0')
      if (x < 0) ans.toInt * -1
      else ans.toInt
    } catch {
      case e: NumberFormatException => 0
    }
  }
}