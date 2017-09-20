package org.range.query.cache

trait Tree[+T]

case class Leaf[T](var value: Option[T], pos: Long) extends Tree[T]

case class Node[T](var value: Option[T], start: Long, end: Long, left: Tree[T], right: Tree[T]) extends Tree[T]

case object Empty extends Tree[Nothing]

/**
 * @author Vipin Kumar
 *
 * Its a predefined  min max range segment tree where we can do query as well as update to particular node
 * with propagation
 */
class SimpleSegmentTreeImpl {

  def build(min_range: Long, max_range: Long, default: Int): Tree[Int] = {

    (min_range == max_range) match {

      case true => Leaf[Int](Option(default), min_range)
      case false => {

        val mid = (min_range + max_range) / 2
        Node[Int](Option(default), min_range, max_range, build(min_range, mid, default), build(mid + 1, max_range, default))

      }

    }
  }

  def update(tree: Tree[Int], leftPos: Int, rightPos: Int, newvalue: Int): Option[Int] = {

    tree match {
      case x: Node[Int] => {

        //completely outside
        (x.start > rightPos && x.end < leftPos) match {

          case true => None
          case false => {

            // range matched
            (x.start == leftPos && x.end == rightPos) match {

              case true => {
                x.value = Option(newvalue)
                x.value
              }
              case false => {
                val res1 = update(x.left, leftPos, rightPos, newvalue)
                val res2 = update(x.right, leftPos, rightPos, newvalue)
                x.value = merge(res1, res2)
                x.value
              }
            }
          }
        }
      }

      case x: Leaf[Int] => {
        (x.pos == leftPos && leftPos == rightPos) match {
          case true => {
            x.value = Option(newvalue)
            x.value
          }
          case false => x.value
        }
      }
    }
  }

  def show(tree: Tree[Int], space: Int, count: Int): Unit = {

    tree match {
      case x: Node[Int] => {
        show(x.right, space + count, count)
        println()
        for (i <- count to space) {
          print(" ")
        }
        println(x.value.get)
        show(x.left, space + count, count)
      }
      case x: Leaf[Int] => {
        for (i <- count to space) {
          print(" ")
        }
        println(x.value.get)
      }
      case _ =>
    }
  }

  def query(tree: Tree[Int], leftPos: Long, rightPos: Long): Option[Int] = {

    tree match {
      case x: Node[Int] => {
        //completely outside range
        (x.start > rightPos || x.end < leftPos) match {
          case true => None
          case false =>
            //Completely within range
            (leftPos == x.start && rightPos == x.end) match {

              case true => x.value match {
                case Some(y) => x.value
                case None => {
                  x.value = Option(getData()) //get data if not found
                  x.value
                }
              }
              //not in  range
              case false => {
                val mid = (leftPos + rightPos) / 2
                val res1 = query(x.left, leftPos, mid)
                val res2 = query(x.right, mid, rightPos)
                merge(res1, res2)
              }
            }
        }
      }

      case x: Leaf[Int] => {
        x.value = Option(getData()) //get data if not found
        x.value
      }
    }

  }

  def getData(): Int = {
    1
  }

  def merge(res1: Option[Int], res2: Option[Int]): Option[Int] = {
    res1 match {
      case Some(x) => {
        res2 match {
          case Some(y) => {
            Option(x + y)
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
    val segTre = new SimpleSegmentTreeImpl
    val root = segTre.build(0, 6, 0)
    segTre.show(root, 1, 10)
    println()
    println(segTre.query(root, 2, 3).get)
    segTre.update(root, 4, 5, 2)
    println()
    segTre.show(root, 1, 10)
  }
}
