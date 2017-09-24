package org.range.query.cache

trait Tree[+T]

case class Leaf[T](var value: Option[T], pos: Long) extends Tree[T]

case class Node[T](var value: Option[T], start: Long, end: Long, left: Tree[T], right: Tree[T]) extends Tree[T]

case object Empty extends Tree[Nothing]

/**
 * @author Vipin Kumar
 *
 *         Its a predefined  min max range segment tree where we can do query as well as update to particular node
 *         with propagation
 */
class SimpleSegmentTreeImpl {

  def build(min_range: Long, max_range: Long): Tree[Int] = {

    (min_range == max_range) match {

      case true => Leaf[Int](None, min_range)
      case false => {

        val mid = (min_range + max_range) / 2
        Node[Int](None, min_range, max_range, build(min_range, mid), build(mid + 1, max_range))

      }

    }
  }

  def update(tree: Tree[Int], idx: Int, newvalue: Int): Unit = {

    tree match {
      case x: Node[Int] => {
        //completely outside
        (x.start == x.end) match {
          case true => x.value = Option(newvalue)

          case false => {
            val mid = (x.start + x.end) / 2
            (x.start <= idx && idx <= mid) match {
              case true => update(x.left, idx, newvalue)
              case false => update(x.right, idx, newvalue)
            }

            x.value = add(x.left, x.right)
          }

        }
      }
      case x: Leaf[Int] => {
        (x.pos == idx) match {
          case true => {
            x.value = Option(newvalue)
          }
          case false => {

          }
        }
      }
    }
  }

  def add(left: Tree[Int], right: Tree[Int]): Option[Int] = {

    left match {

      case x: Node[Int] => {

        right match {

          case a: Node[Int] => {

            Option(x.value.getOrElse(0) + a.value.getOrElse(0))

          }
          case b: Leaf[Int] => {
            Option(x.value.getOrElse(0) + b.value.getOrElse(0))
          }
        }

      }

      case y: Leaf[Int] => {
        right match {

          case a: Node[Int] => {

            Option(y.value.getOrElse(0) + a.value.getOrElse(0))

          }
          case b: Leaf[Int] => {
            Option(y.value.getOrElse(0) + b.value.getOrElse(0))
          }
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
        println(x.value.getOrElse(0))
        show(x.left, space + count, count)
      }
      case x: Leaf[Int] => {
        for (i <- count to space) {
          print(" ")
        }
        println(x.value.getOrElse(0))
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
            (leftPos <= x.start && x.end <= rightPos) match {
              case true => x.value match {
                case Some(y) => {
                  x.value
                }
                case None => {
                  x.value = Option(getData()) //get data if not found
                  x.value
                }
              }
              //not in  range
              case false => {
                val res1 = query(x.left, leftPos, rightPos)
                val res2 = query(x.right, leftPos, rightPos)
                x.value = merge(res1, res2)
                x.value
              }
            }
        }
      }

      case x: Leaf[Int] => {
        (leftPos == x.pos || rightPos == x.pos) match {
          case true => x.value match {
            case Some(y) => x.value
            case None => {
              x.value = Option(getData()) //get data if not found
              x.value
            }
          }
          case false => None
        }
      }

    }
  }

  def getData(): Int = {
    1
  }

  def merge(res1: Option[Int], res2: Option[Int]): Option[Int] = {
    Option(res1.getOrElse(0) + res2.getOrElse(0))
  }
}

object SimpleSegmentTreeImpl {
  def main(args: Array[String]) {
    val segTre = new SimpleSegmentTreeImpl
    val root = segTre.build(0, 6)
    println("----------------------------------------------------------------------------")
    println(segTre.query(root, 2, 3))
    println("----------------------------------------------------------------------------")
    segTre.show(root, 1, 10)
    println("----------------------------------------------------------------------------")
    println(segTre.query(root, 0, 3))
    println("----------------------------------------------------------------------------")
    segTre.update(root, 2, 2)
    segTre.show(root, 1, 10)
    println("----------------------------------------------------------------------------")
    println(segTre.query(root, 5, 6))
    println("----------------------------------------------------------------------------")
    segTre.show(root, 1, 10)
    println(segTre.query(root, 0, 4))
    println("----------------------------------------------------------------------------")
    segTre.show(root, 1, 10)
    println("----------------------------------------------------------------------------")
    println(segTre.query(root, 2, 3))
    println("----------------------------------------------------------------------------")
    segTre.show(root, 1, 10)
  }
}
