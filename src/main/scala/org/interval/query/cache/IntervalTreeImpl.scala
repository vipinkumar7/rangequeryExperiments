package org.interval.query.cache

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

  def isOverlap(i1: Interval, i2: Interval): Boolean = {

    if (i1.low <= i2.high && i2.low <= i1.high)
      return true
    return false
  }

  def searchInterval(root: Node, interval: Interval): Interval = {

    if (root == null) return null;

    if (isOverlap(root.interval, interval))
      return root.interval

    if (root.left != null && root.left.max >= interval.low)
      return searchInterval(root.left, interval)

    return searchInterval(root.right, interval)

  }

  def inorder(root: Node): Unit =
    {
      if (root == null) return ;

      inorder(root.left);

      println("[" + root.interval.low + ", " + root.interval.high + " ]"
        + " max = " + root.max);

      inorder(root.right);
    }

  def main(args: Array[String]): Unit = {

    val intervals = Array(Interval(15, 20), Interval(10, 30),
      Interval(17, 19),
      Interval(5, 20), Interval(12, 15), Interval(30, 40))

    var root = null.asInstanceOf[Node]
    for (i <- 0 to intervals.length - 1) {
      root = insert(root, intervals(i))
    }

    inorder(root);

    val d = searchInterval(root, Interval(6, 7))
    println(d)
  }
}