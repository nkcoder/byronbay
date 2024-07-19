package my.playground
package catscore.typeclass

extension [A](a: A) {
  def encode(using codec: ByteCodec[A]): Array[Byte] = codec.encode(a)
}

extension (bytes: Array[Byte]) {
  def decode[A](using codec: ByteCodec[A]): Option[A] = codec.decode(bytes)
}

object ByteCodecOpsApp extends App {

  println(5.encode.toList)
  println(Array[Byte](0, 0, 0, 6).decode[Int])


  println("hello".encode.toList)
  println(Array[Byte](104, 101, 108, 108, 111).decode[String])
}
