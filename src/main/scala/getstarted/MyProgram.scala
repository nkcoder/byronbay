package my.playground
package getstarted

object MyProgram:
  def abs(n: Int): Int =
    if (n < 0) then -n
    else n

  private def formatAbs(n: Int) =
    val message = "The absolute value of %d is %d"
    message.format(n, abs(n))

  @main def printAbs: Unit =
    val n = -42
    println(formatAbs(n))

