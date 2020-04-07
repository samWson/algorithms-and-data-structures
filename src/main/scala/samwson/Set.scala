package samwson

import scala.collection.mutable.ArrayBuffer

case class Set(private var elements: samwson.Array) {

  def insert(value: Int): Unit = {
    if (!elements.contains(value)) {
      elements.insert(value)
    }
  }

  def delete(value: Int): Int = elements.delete(value)

  def read(index: Int): Int = elements.read(index)

  def sort(): Unit = elements.bubbleSort()
  
  def contains(value: Int): Boolean = elements.contains(value)

  def search(value: Int): Option[Int] = elements.linearSearch(value)

  def length(): Int = elements.length
}
