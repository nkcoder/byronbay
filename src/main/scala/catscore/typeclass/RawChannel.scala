package my.playground
package catscore.typeclass

import java.io.FileOutputStream
import java.nio.ByteBuffer

import scala.util.Using

trait RawChannel {
  def write(obj: Any): Unit
}

/**
 * pros: simple
 * cons: two responsibilities; unhandled exceptions
 *
 */
object FileRawChannel extends RawChannel {
  override def write(obj: Any): Unit = {
    val bytes: Array[Byte] = obj match {
      case n: Int =>
        val buffer = ByteBuffer.allocate(4)
        buffer.putInt(n)
        buffer.array()
      case s: String => s.getBytes
      case _         => throw new IllegalArgumentException("Unsupported type")
    }

    Using(new FileOutputStream("file.txt")) { fos =>
      fos.write(bytes)
      fos.flush()
    }
  }

}

object RawChannelApp extends App {
  FileRawChannel.write(42)
  FileRawChannel.write("Hello, World!")
}
