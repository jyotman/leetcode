package problems100

class ListNode(var _x: Int = 0) {
  var next: ListNode = null
  var x: Int = _x
}

object Problem2 {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {

    def add(l1: ListNode, l2: ListNode, res: ListNode = new ListNode, carry: Int = 0): ListNode = {
      if (l1 == null && l2 == null){
        if(carry == 0)
          null
        else {
          val (unitValue, carryValue) = getUnitAndCarry(carry)
          res.x = unitValue.toInt
          res.next = add(null, null, carry = carryValue.toInt)
          res
        }
      }
      else if (l1 == null) {
        val sum = l2.x + carry
        if(sum > 9) {
          val (unitValue, carryValue) = getUnitAndCarry(sum)
          res.x = unitValue.toInt
          res.next = add(null, l2.next, carry = carryValue.toInt)
        }
        else {
          res.x = sum
          res.next = l2.next
        }
        res
      }
      else if (l2 == null) {
        val sum = l1.x + carry
        if(sum > 9) {
          val (unitValue, carryValue) = getUnitAndCarry(sum)
          res.x = unitValue.toInt
          res.next = add(l1.next, null, carry = carryValue.toInt)
        }
        else {
          res.x = sum
          res.next = l1.next
        }
        res
      }
      else {
        val sum = l1.x + l2.x + carry
        val (unitValue, carryValue) = getUnitAndCarry(sum)
        res.x = unitValue.toInt
        res.next = add(l1.next, l2.next, carry = carryValue.toInt)
        res
      }
    }

    def getUnitAndCarry(num:Int):(String, String) = {
      val numString = num.toString
      val unitValue = numString.last.toString
      val nonUnitValue = {
        val nonUnit = numString.substring(0, numString.length - 1)
        if (nonUnit == "") "0" else nonUnit
      }
      (unitValue, nonUnitValue)
    }

    add(l1, l2)
  }
}
