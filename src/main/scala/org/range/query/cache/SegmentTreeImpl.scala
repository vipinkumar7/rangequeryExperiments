package org.range.query.cache

trait Tree[+T]

case class Leaf[T](value: T, pos: Long) extends Tree[T]

case class Node[T](value: T, start: Long, end: Long, left: Tree[T], right: Tree[T]) extends Tree[T]

case object Empty extends Tree[Nothing]

case class QueryResponse[T](value: T, start: Long, end: Long)

/**
 * @author Vipin Kumar
 */
class SimpleSegmentTreeImpl[T] {

  def build(min_range: Long, max_range: Long, default: T): Tree[T] = {

    (min_range == max_range) match {

      case true => Leaf(default, min_range)

      case false => {
        val mid = (min_range + max_range) / 2
        Node[T](default, min_range, max_range, build(min_range, mid, default), build(mid + 1, max_range, default))
      }

    }
  }


  def show(tree: Tree[T], space: Int, count: Int): Unit = {

    tree match {
      case x: Node[T] => {
        show(x.right, space + count, count)
        println()
        for (i <- count to space) {
          print(" ")
        }
        println(x.value)
        show(x.left, space + count, count)
      }
      case x: Leaf[T] => {
        for (i <- count to space) {
          print(" ")
        }
        println(x.value)
      }
      case _ =>
    }
  }


  def query(tree: Tree[T], leftPos: Long, rightPos: Long): Option[QueryResponse[T]] = {

    tree match {
      case x: Node[T] =>
        //completely outside range
        (x.start > rightPos || x.end < leftPos) match {
          case true => None
          case false =>
            //Completely within range
            (leftPos <= x.start && rightPos >= x.end) match {

              case true => Some(QueryResponse(x.value, x.start, x.end))
              //partially inside partially outside range
              case false => {
                val mid = (leftPos + rightPos) / 2
                val res1 = query(x.left, leftPos, mid)
                val res2 = query(x.right, mid, rightPos)
                merge(res1, res2)
              }
            }
        }
    }
  }

  def merge(res1: Option[QueryResponse[T]], res2: Option[QueryResponse[T]]): Option[QueryResponse[T]] = {
    res1 match {
      case Some(x) => {
        res2 match {
          case Some(y) => {
            res1
          }
          case None =>
            res1
        }
      }
      case None => res2
    }
  }
}

object SimpleSegmentTreeImpl {

  def main(args: Array[String]) {
    val segTre = new SimpleSegmentTreeImpl[Int]
    val root = segTre.build(0, 5, 0)
    segTre.show(root, 1, 10)
  }
}
