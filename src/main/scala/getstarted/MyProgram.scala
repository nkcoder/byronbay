package my.playground
package getstarted

import scala.annotation.tailrec

object MyProgram:
  def abs(n: Int): Int =
    if (n < 0) then -n
    else n

  private def formatAbs(n: Int) =
    val message = "The absolute value of %d is %d"
    message.format(n, abs(n))

  @main def printAbs(): Unit =
    val n = -42
    println(formatAbs(n))

  def factorial(n: Int): Int =
    @tailrec
    def go(n: Int, acc: Int): Int =
      if n <= 0 then acc
      else go(n - 1, n * acc)

    go(n, 1)

  private def formatFactorial(n: Int): String =
    val message = "The factorial value of %d is %d"
    message.format(n, factorial(n))

  def printAbsAndFactorial(): Unit =
    val n = 5
    println(formatAbs(n))
    println(formatFactorial(n))

  private def formatResult(name: String, n: Int, f: Int => Int): String =
    val message = "The %s of %d is %d"
    message.format(name, n, f(n))

  def formatAbs2(n: Int): String =
    formatResult("abs", n, abs)

  def formatFactorial2(n: Int): String =
    formatResult("factorial", n, factorial)

  def fibonacci(n: Int): Int =
//    @tailrec
    def go(n: Int): Int =
      if n == 1 then 0
      else if n == 2 then 1
      else go(n - 1) + go(n - 2)
    go(n)
