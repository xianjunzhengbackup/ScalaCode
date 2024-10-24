package testMyCode

/*
 You are given an integer array cost where cost[i] is the cost of ith step on a staircase. Once you pay the cost, you can either climb one or two steps.

You can either start from the step with index 0, or the step with index 1.

Return the minimum cost to reach the top of the floor.

 

Example 1:

Input: cost = [10,15,20]
Output: 15
Explanation: You will start at index 1.
- Pay 15 and climb two steps to reach the top.
The total cost is 15.

Example 2:

Input: cost = [1,100,1,1,1,100,1,1,100,1]
Output: 6

Hint 1
Build an array dp where dp[i] is the minimum cost to climb to the top starting from the ith staircase.
Hint 2
Assuming we have n staircase labeled from 0 to n - 1 and assuming the top is n, then dp[n] = 0, marking that if you are at the top, the cost is 0.
Hint 3
Now, looping from n - 1 to 0, the dp[i] = cost[i] + min(dp[i + 1], dp[i + 2]). The answer will be the minimum of dp[0] and dp[1]
 */

object climbingStairApp extends App{
  val cost=List(1,100,1,1,1,100,1,1,100,1)
  val h = cost.size
  val arrayPath: Array[Int]= new Array[Int](h)
  val H1 = h-1
  val H2 = h-2

  for(i <- H1 to 0 by -1){
    i match{
      case `H1` => arrayPath(i) = cost(i)
      case `H2` => arrayPath(i) = cost(i)
      case _ => {
        val minCost = if(arrayPath(i+1) < arrayPath(i+2)) arrayPath(i+1);else arrayPath(i+2)
        arrayPath(i) = cost(i) + minCost
      }
    }
  }
  arrayPath foreach println
  if(arrayPath(0) < arrayPath(1)) println(s"It need to start from 0, and total cost is ${arrayPath(0)}")
  else println(s"It needs to start from 1, and totoal cost is ${arrayPath(1)}")
}
