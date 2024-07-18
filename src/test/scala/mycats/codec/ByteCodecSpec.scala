package my.playground
package mycats.codec

import mycats.codec.ByteCodec.{IntByteCodecLaws, StringByteCodecLaws}

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

trait ByteCodecTests[A] extends Laws {
  def laws: ByteCodecLaws[A]

  def byteCodec(using arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
    name = "ByteCodec",
    parent = None,
    props = "ismonomorphic" -> forAll(laws.ismonomorphic(_))
  )
}

object ByteCodecTests {
  def apply[A](using defaultCodec: ByteCodec[A]): ByteCodecTests[A] = new ByteCodecTests[A] {
    def laws: ByteCodecLaws[A] = new ByteCodecLaws[A] {
      override def codec: ByteCodec[A] = defaultCodec
    }
  }
}

object IntByteCodecTests extends ByteCodecTests[Int] {
  def laws: ByteCodecLaws[Int] = IntByteCodecLaws
}

object StringByteCodecTests extends ByteCodecTests[String] {
  def laws: ByteCodecLaws[String] = StringByteCodecLaws
}

class ByteCodecSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {

  checkAll("ByteCodec[Int]", IntByteCodecTests.byteCodec)
  checkAll("ByteCodec[String]", StringByteCodecTests.byteCodec)

  // using the `apply` method
  checkAll("ByteCodec[Int] (apply)", ByteCodecTests[Int].byteCodec)
  checkAll("ByteCodec[String] (apply)", ByteCodecTests[String].byteCodec)
}
