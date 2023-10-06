package my.playground
package fpinscala.datastructures

import fpinscala.datastructures.Tree.{Branch, Leaf}

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(v)      => 1
    case Branch(l, r) => 1 + l.size + r.size

  // 3.26
  def depth: Int = this match
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + l.depth.max(r.depth)

  // 3.27
  def map[B](f: A => B): Tree[B] = this match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  // 3.28 generalize `size`, `maximum`, `depth`, `map`, writing a new function `fold` that abstracts over their similarities. Reimplement them in terms of this more general function. (todo)
  def fold[B](f: A => B, g: (B, B) => B): B = this match
    case Leaf(v)      => f(v)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  // 3.28
  def sizeViaFold: Int                   = fold(a => 1, 1 + _ + _)
  def depthViaFold: Int                  = fold(a => 0, 1 + _.max(_))
  def mapViaFold[B](f: A => B): Tree[B]  = fold(a => Leaf(f(a)), (l, r) => Branch(l, r))
  def mapViaFold2[B](f: A => B): Tree[B] = fold(a => Leaf(f(a)), Branch(_, _))

object Tree:
  // 3.25 write a function `maximum` to return the maximum element in a Tree[Int] (use x.max(y))
  extension (t: Tree[Int])
    def maximum: Int =
      t match
        case Leaf(v)      => v
        case Branch(l, r) => l.maximum.max(r.maximum)

    // 3.28 (todo)
    def maximumViaFold: Int  = t.fold(a => a, (l, r) => l.max(r))
    def maximumViaFold2: Int = t.fold(a => a, _ max _)
    def maximumViaFold3: Int = t.fold(a => a, _.max(_))

  // 3.26 write a function `depth` to return the maximum path length from the root of a tree to any leaf.
  def depth[A](tree: Tree[A]): Int = tree match
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l).max(depth(r)))

  // 3.27 write a function `map` that modifies each element in a tree with a given function
  def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
