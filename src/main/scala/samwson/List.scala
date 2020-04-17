package samwson

class Node(val value: Int) {
  var next = None: Option[Node]

  override def toString(): String = s"Node($value)"
}

class List() {
  private var firstNode = None: Option[Node]

  def append(value: Int): this.type = {
    def newNode(value: Int, node: Node): Unit = {
      node.next match {
        case None => node.next = Option(new Node(value))
        case Some(n) => newNode(value, n)
      }
    }

    firstNode match {
      case Some(node) => newNode(value, node)
      case None => firstNode = Option(new Node(value))
    }

    this
  }

  def read(index: Int = length - 1): Option[Int] = {
    def readNode(node: Node, index: Int): Option[Int] = {
      index match {
        case 0 => Some(node.value)
        case i => readNode(node.next.get, i - 1)
      }
    }

    firstNode match {
      case None => None
      case Some(node) => readNode(node, index)
    }
  }


  def length(): Int = {
    def countNodes(node: Node, tally: Int = 1): Int = node.next match {
      case None => tally
      case Some(n) => countNodes(n, tally + 1)
    }

    if (firstNode.isEmpty) 0 else countNodes(firstNode.get)
  }

  def indexOf(value: Int): Option[Int] = {
    def searchNodes(node: Node, value: Int, index: Int = 0): Option[Int] = node.value match {
      case x if x == value => Some(index)
      case _ => if (node.next.isEmpty) None else searchNodes(node.next.get, value, index + 1)
    }

    firstNode match {
      case None => None
      case Some(node) => searchNodes(node, value)
    }
  }

  def delete(index: Int): Option[Int] = {
    def deleteNode(previous: Node, current: Node): Option[Int] = {
      val value = current.value

      if (current.next.isEmpty) {
        previous.next = None
      } else {
        previous.next = current.next
      }

      Some(value)
    }

    def traverseNodes(previous: Node, current: Node, index: Int): Option[Int] = index match {
      case 0 => deleteNode(previous, current)
      case i => if (current.next.isEmpty) None else traverseNodes(current, current.next.get, i - 1)
    }

    index match {
      case 0 => {
        val value = if (firstNode.isEmpty) {
          None
        } else {
          Some(firstNode.get.value)
        }

        firstNode = firstNode.get.next
        value
      }
      case i if firstNode.get.next.nonEmpty => traverseNodes(firstNode.get, firstNode.get.next.get, i - 1)
    }
  }
}
