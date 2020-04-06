package samwson

import scala.collection.mutable.ArrayBuffer

// TODO: Replace ArrayBuffer with new samwson.Array.
case class Set[A](private var elements: ArrayBuffer[A]) {

  def insert(value: A): Unit = {
    if (!elements.contains(value)) {
      elements.append(value)
    }
  }

  def delete(value: A): Unit = elements -= value

  def read(index: Int): A = elements(index)

  def search(value: A): Boolean = elements.contains(value)

  def length(): Int = elements.length
}
