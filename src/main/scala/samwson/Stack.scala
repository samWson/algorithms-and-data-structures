package samwson

case class Stack(private val elements: samwson.Array) {
  def read(): Int = elements.read(elements.length - 1)

  def push(value: Int): Unit = elements.insert(value)

  def pop(): Int = elements.delete(elements.length - 1)

  def size(): Int = elements.length()
}
