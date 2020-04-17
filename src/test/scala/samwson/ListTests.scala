package samwson

import org.scalatest.funspec.AnyFunSpec
import scala.language.reflectiveCalls

class ListSpec extends AnyFunSpec {
  def fixture() = new {
    val list = new samwson.List()
      .append(5)
      .append(2)
      .append(7)
  }

  describe("List::read") {
    describe("With no index argument") {
      val f = fixture()

      it("Defaults to reading the last index of the list") {
        assertResult(Some(7)) (f.list.read())
      }
    }

    describe("With an index argument") {
      val f = fixture()

      it("Returns the value at the index") {
        assertResult(Some(2)) (f.list.read(1))
      }
    }

    describe("With an empty list") {
      it("Returns None") {
        assertResult(None) (new samwson.List().read())
      }
    }
  }

  describe("List::indexOf") {
    describe("The searched value is in the list") {
      val f = fixture()

      it("Returns the index of the first instance of the value") {
        assertResult(Some(2)) (f.list.indexOf(7))
      }
    }

    describe("The searched value is not in the list") {
      val f = fixture()

      it("Returns None") {
        assertResult(None) (f.list.indexOf(10))
      }
    }
  }

  describe("List::append") {
    val f = fixture()

    it("Appends the value to the end of the list") {
      f.list.append(10)

      assertResult(Some(10)) (f.list.read())
      assertResult(4) (f.list.length())
    }
  }

  describe("List::delete") {
    describe("The index to delete is at the end of the list") {
      val f = fixture()

      it("Deletes and returns the value from deleted index") {
        assertResult(Some(7)) (f.list.delete(2))
        assertResult(2, "Length expected to be 2") (f.list.length())
      }
    }

    describe("The index to delete is in the middle of the list") {
      val f = fixture()

      it("Deletes and returns the value from the deleted index") {
        assertResult(Some(2)) (f.list.delete(1))
        assertResult(2, "Length expected to be 2") (f.list.length())
        assertResult(Some(7)) (f.list.read())
      }
    }

    describe("The index to delete is at the beginning of the list") {
      val f = fixture()

      it("Deletes and returns the value from the deleted index") {
        assertResult(Some(5)) (f.list.delete(0))
        assertResult(2) (f.list.length())
        assertResult(Some(2)) (f.list.read(0))
      }
    }
  }
}
