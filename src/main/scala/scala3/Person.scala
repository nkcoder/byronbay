package my.playground
package scala3

class Person(val name: String, age: Int) {
  println(s"Person $name created")
}

object Person extends App {
  private val john = new Person("John", 25)
  println(john.name)
}