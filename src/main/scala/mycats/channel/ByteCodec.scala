package my.playground
package mycats.channel

import scala.util.Try

/**
 * Can be instanced by any type; Cleaner interface; Several implementations.
 */
trait TypeChannel {
  def write[A](a: A)(encoder: ByteEncoder[A]): Unit
}

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  def apply[A](using encoder: ByteEncoder[A]): ByteEncoder[A] = encoder

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = (a: A) => f(a)

  given ByteEncoder[Int] with {
    override def encode(a: Int): Array[Byte] = {
      val buffer = java.nio.ByteBuffer.allocate(4)
      buffer.putInt(a)
      buffer.array()
    }
  }

  given ByteEncoder[String] with {
    override def encode(a: String): Array[Byte] = a.getBytes
  }

}

object FileTypeChannel extends TypeChannel {
  import java.io.FileOutputStream

  import scala.util.Using

  override def write[A](a: A)(using encoder: ByteEncoder[A]): Unit =
    Using(new FileOutputStream("file.txt")) { fos =>
      fos.write(encoder.encode(a))
      fos.flush()
    }
}

trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

object ByteDecoder {
  def apply[A](using decoder: ByteDecoder[A]): ByteDecoder[A] = decoder

  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = (bytes: Array[Byte]) => f(bytes)

  given ByteDecoder[String] with {
    override def decode(bytes: Array[Byte]): Option[String] =
      Try(new String(bytes)).toOption
  }
}

/**
 * Testing
 */
object TypeChannelApp extends App {
  FileTypeChannel.write(45)
  FileTypeChannel.write("Hello, type2!")

  val data: Array[Byte] = Array(72, 101, 108, 108, 111)
  val decoded           = ByteDecoder[String].decode(data)
  assert(decoded.contains("Hello"))
}
