package my.playground
package fpinscala.datastructures

import fpinscala.datastructures.Tree.{Branch, Leaf}

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:
  // 3.25
  def maximum(tree: Tree[Int]): Int =
    tree match
      case Leaf(v) => v
      case Branch(l, r) =>
        val lx = maximum(l)
        val rx = maximum(r)
        lx.max(rx)

  // 3.26
  def depth[A](tree: Tree[A]): Int = ???

  // 3.27
  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = ???

  // 3.28
  def fold[A, B](tree: Tree[A], f: (A, B) => B): B = ???
