package testMyCode
import scala.collection.mutable

/*
 1. Two Sum

Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target. You may assume that each input would have exactly one solution, and you may not use the same element twice. You can return the answer in any order.

Example 1:
Input: nums = [2,7,11,15], target = 9
Output: [0,1]
Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].

Example 2:
Input: nums = [3,2,4], target = 6
Output: [1,2]

Example 3:
Input: nums = [3,3], target = 6
Output: [0,1]

 */

object TwoSumApp extends App {
  def twoSum(nums: Array[Int],target: Int): Array[Int]={
    val cache = mutable.Map[Int,Int]()
    nums.zipWithIndex.find {
      case (value,index)=> cache.get(target - value) match {
        case Some(_) =>true
        case _ => cache.put(value,index).isDefined
      }
    } match {
      case Some((num,index)) => Array(cache(target - num),index)
      case _ => throw new IllegalArgumentException("invalid parameter,no valid result")
    }
  }

  twoSum(Array(2,7,11,15),9) foreach println
}
