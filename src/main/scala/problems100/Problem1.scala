package problems100

object Problem1 {

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val numsMap = nums.indices.groupBy(i => nums(i))

    def getPair(index: Int = 0): Array[Int] = {
      if (index == nums.length)
        return Array()
      val num = nums(index)
      val numWanted = target - num
      if (num == numWanted && numsMap(num).length >= 2)
        numsMap(num).toArray take 2
      else if (numsMap.contains(numWanted) && numsMap(numWanted).head != index)
        Array(numsMap(num).head, numsMap(numWanted).head)
      else
        getPair(index + 1)
    }

    getPair()
  }
}