package drzewo

import scala.annotation.tailrec //@tailrec

trait Tree[A] {
 //lenght of the longest branch of the tree from the root
  def height(t: Tree[A]): Int = {
    @tailrec
    def iter(ts: List[Tree[A]], h: Int, hs: List[Int]): Int = ts match {
      case Nil => hs.max
      case t :: ts => t match {
        case Leaf(_) => iter(ts, 1, h::hs)
        case Node(_,c) => iter(ts++c, h+1, hs)
      }
    }
    iter(List(t), 0, List(0))
  }

//number of all segments of the tree
  def size(t: Tree[A]): Int = {
    @tailrec
    def iter(ts: List[Tree[A]], s: Int): Int = ts match {
      case Nil => s
      case t :: ts => t match {
        case Leaf(_) => iter(ts, s+1)
        case Node(_,c) => iter(ts++c, s+1)
      }
    }
    iter(List(t), 0)
  }

//number of leaves of the tree
  def leaves(t: Tree[A]): Int = {
    @tailrec
    def iter(ts: List[Tree[A]], l: Int): Int = ts match {
      case Nil => l
      case t :: ts => t match {
        case Leaf(_) => iter(ts, l+1)
        case Node(_,c) => iter(ts++c, l)
      }
    }
    iter(List(t), 0)
  }

}



case class Leaf[A](val value: A) extends Tree[A]
case class Node[A](val value: A, val children: List[Tree[A]]) extends Tree[A]

object Main extends App {
  type A = Int

  val t1: Tree[A] = Node(1, List(Node(2, List(Leaf(5), Leaf(6))), Node(3, List(Node(7, List(Node(9, List(Node(10, List(Node(11, List(Leaf(14))))))), Node(12, List(Node(13, List(Leaf(15))))))))), Node(4, List(Leaf(8)))))
  val t2: Tree[A] = Node(1, List(Leaf(2), Leaf(3), Node(4, List(Leaf(5)))))
  println("\ntree: " + t1)
  println("\nsize: " + t1.size(t1))
  println("\nheight: " + t1.height(t1))
  println("\nleaves: " + t1.leaves(t1))
  println("\n")
  println("\ntree: " + t2)
  println("\nsize: " + t2.size(t2))
  println("\nheight: " + t2.height(t2))
  println("\nleaves: " + t2.leaves(t2))

}
