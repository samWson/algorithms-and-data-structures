package samwson

class Queue() {
  private val elements = new samwson.DoublyLinkedList()

  def length(): Int = elements.length()

  def enqueue(value: Int): this.type = { elements.append(value); this }

  def dequeue(): Int = elements.delete()

  def read(): Int = elements.read(0)
}
