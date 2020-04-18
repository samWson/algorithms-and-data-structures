package samwson

class DoublyLinkedList() {
  private var firstNode = None: Option[DoublyLinkedNode]
  private var lastNode = None: Option[DoublyLinkedNode]

  private class DoublyLinkedNode(val value: Int) {
    var previous = None: Option[DoublyLinkedNode]
    var next = None: Option[DoublyLinkedNode]
  }

  def append(value: Int): this.type = {
    val newNode = Some(new DoublyLinkedNode(value))

    def traverseNodes(current: DoublyLinkedNode, newNode: Some[DoublyLinkedNode]): Unit = current.next match {
      case None => {
        current.next = newNode
        newNode.get.previous = Some(current)
      }
      case Some(_) => {
        traverseNodes(current.next.get, newNode)
      }
    }

    if (firstNode.isEmpty) {
      firstNode = newNode
      lastNode = newNode
    } else {
      traverseNodes(firstNode.get, newNode)
    }

    this
  }

  def length(): Int = {
    def traverseNodes(node: DoublyLinkedNode, count: Int = 1): Int = node.next match {
      case None => count
      case Some(_) => traverseNodes(node.next.get, count + 1)
    }

    if (firstNode.isEmpty) {
      0
    } else {
      traverseNodes(firstNode.get)
    }
  }

  def read(index: Int): Int = {
    def traverseNodes(node: DoublyLinkedNode, index: Int): Int = index match {
      case 0 => node.value
      case _ => traverseNodes(node.next.get, index - 1)
    }

    traverseNodes(firstNode.get, index)
  }

  def delete(): Int = {
    val removedNode = firstNode
    firstNode = removedNode.get.next
    removedNode.get.value
  }
}
