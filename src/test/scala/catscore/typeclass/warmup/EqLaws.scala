package my.playground
package catscore.typeclass.warmup

trait EqLaws[A] {
  def eq: Eq[A]

  def reflexivity(x: A): Boolean = eq.eqv(x, x)

  def symmetry(x: A, y: A): Boolean = eq.eqv(x, y) == eq.eqv(y, x)

  def transitivity(x: A, y: A, z: A): Boolean = {
    val xy = eq.eqv(x, y)
    val yz = eq.eqv(y, z)
    val xz = eq.eqv(x, z)
    !(xy && yz) || xz
  }

}
