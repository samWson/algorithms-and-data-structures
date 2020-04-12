package samwson

case class Queue(private val elements: samwson.Array) {
  def length(): Int = elements.length()

  def enqueue(value: Int): Unit = elements.insert(value)

  def dequeue(): Int = elements.delete(0)

  def read(): Int = elements.read(0)
}
