package org.range.query.cache

trait Tree[+T]

case class Leaf[T](value: T, pos: Long) extends Tree[T]

case class Node[T](value: T, start: Long, end: Long, left: Tree[T], right: Tree[T]) extends Tree[T]

case object Empty extends Tree[Nothing]

case class QueryResPonse[T](value: T, start: Long, end: Long)

/**
 * @author Vipin Kumar
 */
class SegmentTreeImpl[T](min_range: Long, max_range: Long) {

  def build(tree: Tree[T], value: T) {

  }

  def query(tree: Tree[T], leftPos: Long, rightPos: Long): Option[QueryResPonse[T]] = {

    tree match {
      case x: Node[T] =>
        //completely outside range
        (x.start > rightPos || x.end < leftPos) match {

          case true => None
          case false =>
            //Completely within range
            (leftPos <= x.start && rightPos >= x.end) match {

              case true => Some(QueryResPonse(x.value, x.start, x.end))

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

  def merge(res1: Option[QueryResPonse[T]], res2: Option[QueryResPonse[T]]): Option[QueryResPonse[T]] = {
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