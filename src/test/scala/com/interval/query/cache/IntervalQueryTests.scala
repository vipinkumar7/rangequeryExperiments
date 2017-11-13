package com.interval.query.cache

import org.scalatest._
import org.interval.query.cache._
import scala.collection.mutable.ListBuffer

class IntervalQueryTests extends FlatSpec {

  "This " should "print one interval" in {

    val intervals = Array(Interval(15, 20), Interval(10, 30),
      Interval(17, 19),
      Interval(5, 20), Interval(12, 15), Interval(30, 40))

    var root = null.asInstanceOf[Node]
    for (i <- 0 to intervals.length - 1) {
      root = IntervalTreeImpl.insert(root, intervals(i))
    }

    val allIn = scala.collection.mutable.ListBuffer.empty[Interval]
    IntervalTreeImpl.inorder(root, allIn);
    IntervalTreeImpl.printTree(allIn)

    val buf = scala.collection.mutable.ListBuffer.empty[Interval]
    val d = IntervalTreeImpl.searchInterval(root, Interval(6, 16), buf)
    buf.foreach { x => println(x.low + " :: " + x.high) }
  }

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

    val buf = scala.collection.mutable.ListBuffer.empty[Interval]
    val d = IntervalTreeImpl.searchInterval(root, Interval(15, 29), buf)
    buf.foreach { x => println(x.low + " :: " + x.high) }
  }
}