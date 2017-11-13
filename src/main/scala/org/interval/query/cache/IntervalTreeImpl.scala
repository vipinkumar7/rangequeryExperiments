package org.interval.query.cache

import scala.collection.mutable.ListBuffer

case class Interval(val low: Long, val high: Long)

case class Node(var interval: Interval, var max: Long, var left: Node, var right: Node)

object IntervalTreeImpl {

  def insert(root: Node, interval: Interval): Node = {
    if (root == null)
      return Node(interval, interval.high, null, null)

    val root_low = root.interval.low
    if (interval.low < root_low) {
      root.left = insert(root.left, interval)
    } else {
      root.right = insert(root.right, interval)
    }
    if (root.max < interval.high) {
      root.max = interval.high
    }

    return root;
  }

  def isOverlap(root_interval: Interval, interval: Interval): Boolean = {

    if (root_interval.low >= interval.low && root_interval.high <= interval.high)
      return true
    return false
  }

  def searchInterval(root: Node, interval: Interval, intervals: ListBuffer[Interval]): Unit = {

    if (root == null) return ;
    println(root.interval.low + "   :   " + root.interval.high)
    if (isOverlap(root.interval, interval)) {
      intervals += (root.interval)
    }

    if (root.left != null && root.left.max >= interval.low)
      searchInterval(root.left, interval, intervals)

    searchInterval(root.right, interval, intervals)

  }

  def inorder(root: Node, allNodes: ListBuffer[Interval]): Unit =
    {
      if (root == null) return ;

      inorder(root.left, allNodes);
      allNodes += root.interval
      inorder(root.right, allNodes);
    }

  def printTree(allNodes: ListBuffer[Interval]): Unit = {

    var mid =allNodes.size / 2;
    var sta = mid-1
    var end = mid+1
    println("[" + allNodes(mid).low + ", " + allNodes(mid).high + " ]")
    while (sta >= 0 && end < allNodes.size - 1) {
     
      println("[" + allNodes(sta).low + ", " + allNodes(sta).high + " ]" + "<->"
        + "[" + allNodes(end).low + ", " + allNodes(end).high + " ]")
      sta = sta - 1
      end = end + 1
    }
    
    while(sta>=0){
      println("[" + allNodes(sta).low + ", " + allNodes(sta).high + " ]");
       sta=sta-1
    }
  }
}