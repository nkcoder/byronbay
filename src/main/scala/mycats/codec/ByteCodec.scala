package my.playground
package mycats.codec

import java.nio.ByteBuffer

import scala.util.Try

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

trait ByteCodec[A] extends ByteDecoder[A] with ByteEncoder[A]

trait ByteCodecLaws[A] {
  def codec: ByteCodec[A]

  def ismonomorphic(a: A): Boolean = {
    val encoded = codec.encode(a)
    val decoded = codec.decode(encoded)
    println(s"encoded: ${encoded.toList}, decoded: $decoded, a: $a, contains: ${decoded.contains(a)}")
    codec.decode(codec.encode(a)).contains(a)
  }
}

object ByteCodec {
  given intByteCodec: ByteCodec[Int] = new ByteCodec[Int] {
    override def decode(bytes: Array[Byte]): Option[Int] =
      if bytes.length != 4 then None
      else
        val bb = ByteBuffer.allocate(4)
        bb.put(bytes)
        bb.flip()
        Some(bb.getInt)

    override def encode(a: Int): Array[Byte] = {
      val buffer = java.nio.ByteBuffer.allocate(4)
      buffer.putInt(a)
      buffer.array()
    }
  }

  object IntByteCodecLaws extends ByteCodecLaws[Int] {
    override def codec: ByteCodec[Int] = intByteCodec
  }

  given stringByteCodec: ByteCodec[String] = new ByteCodec[String] {
    override def decode(bytes: Array[Byte]): Option[String] = Try(new String(bytes)).toOption

    override def encode(a: String): Array[Byte] = a.getBytes
  }

  object StringByteCodecLaws extends ByteCodecLaws[String]:
    override def codec: ByteCodec[String] = stringByteCodec

  given optionByteCodec[A](using codec: ByteCodec[A]): ByteCodec[Option[A]] = new ByteCodec[Option[A]] {
    override def decode(bytes: Array[Byte]): Option[Option[A]] =
      if bytes.isEmpty then Some(None)
      else codec.decode(bytes).map(Some(_))

    override def encode(a: Option[A]): Array[Byte] = a.fold(Array[Byte]())(codec.encode)
  }

  object OptionByteCodecLaws extends ByteCodecLaws[Option[Int]] {
    override def codec: ByteCodec[Option[Int]] = optionByteCodec[Int]
  }

}
