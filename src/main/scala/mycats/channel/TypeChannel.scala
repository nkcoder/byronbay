package my.playground
package mycats.channel

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

object TypeChannelApp extends App {
  FileTypeChannel.write(45)

  FileTypeChannel.write("Hello, type2!")
}
