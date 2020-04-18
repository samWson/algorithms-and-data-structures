package samwson

import org.scalatest.funspec.AnyFunSpec
import scala.language.reflectiveCalls

class DoublyLinkedListSpec extends AnyFunSpec {
  def fixture() = new {
    val list = new samwson.DoublyLinkedList()
      .append(5)
      .append(2)
      .append(7)
  }

  describe("DoublyLinkedList::append") {
    val f = fixture()

    it("Adds the value to the end of the list") {
      f.list.append(10)

      assertResult(4) (f.list.length())
      assertResult(10) (f.list.read(f.list.length - 1))
    }
  }

  describe("DoublyLinkedList::delete") {
    val f = fixture()

    it("Removes and returns the value from the front of the list") {
      assertResult(5, "delete() returns 5") (f.list.delete())
      assertResult(2, "The first value of the list is 2") (f.list.read(0))
      assertResult(2, "The length is reduced by 1") (f.list.length())
    }
  }
}
