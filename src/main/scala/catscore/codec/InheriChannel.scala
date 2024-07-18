package my.playground
package catscore.codec

trait ByteEncodable {
  def encode: Array[Byte]
}

trait InheriChannel {
  def write(obj: ByteEncodable): Unit
}

/**
 * Unique responsibility; Rasy to test;
 * How to extend Int? Only one implementation
 */
object FileInheriChannel extends InheriChannel {
  import java.io.FileOutputStream

  import scala.util.Using

  override def write(obj: ByteEncodable): Unit =
    Using(new FileOutputStream("file.txt")) { fos =>
      fos.write(obj.encode)
      fos.flush()
    }
}

case class FullName(firstName: String, lastName: String) extends ByteEncodable {
  override def encode: Array[Byte] = s"$firstName $lastName".getBytes
}

object InheriChannelApp extends App {
  FileInheriChannel.write(FullName("John", "Doe"))
}
