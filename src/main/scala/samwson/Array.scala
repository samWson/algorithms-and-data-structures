package samwson

import scala.collection.mutable.ArrayBuffer

/** Class Array is a wrapper around ArrayBuffer for simplicity. This class exists
  * to allow a variety of algorithms to be made for common tasks e.g. sorting.
  *
  * It can be reused as an internal buffer for other classes in this repository
  * e.g. Set.
  */
case class Array(private val elements: ArrayBuffer[Int]) {

  def insert(value: Int): Unit = elements += value

  def read(index: Int): Int = elements(index)

  def delete(index: Int): Int = elements.remove(index)

  def length(): Int = elements.length

  /** contains() makes use of linearSearch() and so the Array should be sorted before
    * use or it may not work correctly.
    */
  def contains(value: Int): Boolean = {
    this.linearSearch(value) match {
      case Some(_) => true
      case None => false
    }
  }

  /** linearSearch() is O(N) in time. It takes advantage of a sorted Array and may
    * not work correctly with an unsorted Array.
    *
    * Returns the index of the searched value if it is in the Array.
    */
  def linearSearch(value: Int): Option[Int] = {
    linearSearch(value, this.elements)
  }

  private def linearSearch(value: Int, array: ArrayBuffer[Int], index: Int = 0): Option[Int] = {
    try {
      array.head match {
        case element if element == value => Some(index)
        case element if element != value => {
          if (element > value) {
            None
          } else {
            linearSearch(value, array.tail, index + 1)
          }
        }
      }
    } catch {
      case e: NoSuchElementException => None
    }
  }

  /** bubbleSort() is O(N^2) in time.
   */
  def bubbleSort(): Unit = {
    var unsortedUntilIndex = elements.length - 1
    var isSorted = false

    while (!isSorted) {
      isSorted = true

      for (index <- new Range.Exclusive(0, unsortedUntilIndex, 1)) {
        if (elements(index) > elements(index + 1)) {
          isSorted = false
          val temp = elements(index)
          elements(index) = elements(index + 1)
          elements(index + 1) = temp
        }
      }

      unsortedUntilIndex -= 1
    }
  }
}
