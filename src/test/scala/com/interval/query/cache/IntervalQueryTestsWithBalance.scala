package com.interval.query.cache

import org.interval.query.cache.{Interval, IntervalTreeImpl, Node}
import org.scalatest.FlatSpec

class IntervalQueryTestsWithBalance extends FlatSpec{

  "This" should "print more intervals" in {

    val intervals = Array(Interval(10, 15), Interval(11, 16),
      Interval(12, 17), Interval(13, 19), Interval(15, 25),
      Interval(20, 21), Interval(23, 25), Interval(24, 29), Interval(30, 35))

    var root = null.asInstanceOf[Node]
    for (i <- 0 to intervals.length - 1) {
      root = IntervalTreeImpl.insert(root, intervals(i))
    }


    val allIn = scala.collection.mutable.ListBuffer.empty[Interval]
    IntervalTreeImpl.inorder(root, allIn);
    IntervalTreeImpl.printTree(allIn)

    val new_root=IntervalTreeImpl.rebalance_tree(allIn)

    val buf = scala.collection.mutable.ListBuffer.empty[Interval]
    IntervalTreeImpl.searchInterval(new_root, Interval(17, 23), buf)
    buf.foreach { x => println(x.low + " :: " + x.high) }
  }

}
