package my.playground
package scala3

import scala.annotation.tailrec

case class Contract(id: Int, name: String, amount: BigDecimal) extends Comparable[Contract] {
  override def compareTo(that: Contract): Int = this.amount.compareTo(that.amount)
}

class Basic

object Basic extends App {

  def greeting(name: String, age: Int): String = s"Hi, my name is $name and I am $age years old"

  def factorial(n: Int): Int =
    if n == 0 then 1
    else n * factorial(n - 1)

  def fibonacci(n: Int): Int =
    if n <= 1 then n
    else fibonacci(n - 1) + fibonacci(n - 2)

  def isPrime(n: Int): Boolean =
    if n <= 1 then false
    else if n == 2 then true
    else !(2 to Math.sqrt(n).toInt).exists(x => n % x == 0)

  def concat(s: String, n: Int): String = {
    @tailrec
    def helper(s: String, n: Int, acc: String): String =
      if n == 0 then acc
      else helper(s, n - 1, acc + s)

    helper(s, n, "")
  }

  def fibonacci2(n: Int): Int = {
    @tailrec
    def helper(n: Int, a: Int, b: Int): Int =
      if n == 0 then a
      else helper(n - 1, b, a + b)

    helper(n, 0, 1)
  }

  val aNumber = "456";
  aNumber.toIntOption

  val name = "Daniel"
  val speed = 134.2f
  val myth = f"$name can eat $speed%5.2f burgers per minute"
  println(myth)
}
