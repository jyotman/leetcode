package problems100

object Problem4 {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    val combinedLength = nums1.length + nums2.length
    val medianIndex = combinedLength / 2

    def find(nums1: Array[Int], nums2: Array[Int], i: Int = 0, j: Int = 0, last: Int = 0): Double = {
      val (next, iNext, jNext) = getNext(nums1, nums2, i, j)

      if (i + j == medianIndex) {
        if (combinedLength % 2 != 0) next
        else (last + next) / 2.0
      }
      else find(nums1, nums2, iNext, jNext, next)
    }

    def getNext(nums1: Array[Int], nums2: Array[Int], i: Int, j: Int): (Int, Int, Int) = {
      if (i < nums1.length) {
        if (j < nums2.length) {
          if (nums1(i) < nums2(j)) (nums1(i), i + 1, j)
          else (nums2(j), i, j + 1)
        }
        else (nums1(i), i + 1, j)
      }
      else (nums2(j), i, j + 1)
    }

    find(nums1, nums2)
  }
}