package samwson

import org.scalatest.funspec.AnyFunSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

class ArraySpec extends AnyFunSpec {

  def fixture() = new {
    val array = samwson.Array(ArrayBuffer(1, 8, 4, 5, 3, 9, 7, 2, 6))
    val sortedArray = samwson.Array(ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  describe("Array::insert") {
    val f = fixture()

    it("Adds the value to the array") {
      val lengthBeforeInsertion = f.array.length()

      f.array.insert(10)

      assertResult(true) {
        f.array.bubbleSort()
        f.array.contains(10)
      }
    }
  }

  describe("Array::delete") {
    val f = fixture()

    it("Removes the value and returns the value at the index") {
      val lengthBeforeDeletion = f.array.length()

      val deletedValue = f.array.delete(2)

      assertResult(false) {
        f.array.bubbleSort()
        f.array.contains(deletedValue)
      }

      assertResult(4) (deletedValue)
    }
  }

  describe("Array::read") {
    val f = fixture()

    it("Returns the value at the read index") {
      assertResult(4) (f.array.read(2))
    }
  }

  describe("Array::contains") {
    val f = fixture()

    describe("The value is in the array") {
      it("Returns true") {
        assertResult(true) {
          f.array.bubbleSort()
          f.array.contains(4)
        }
      }
    }

    describe("The value is not in the array") {
      it("Returns false") {
        assertResult(false) {
          f.array.bubbleSort()
          f.array.contains(10)
        }
      }
    }
  }

  describe("Array::linearSearch") {
    val f = fixture()

    describe("The value is in the array") {
      it("Returns the index of the first matching value") {
        assertResult(Some(3)) {
          f.array.bubbleSort()
          f.array.linearSearch(4)
        }
      }
    }

    describe("The value is not in the array") {
      it("Returns None") {
        assertResult(None) {
          f.array.bubbleSort()
          f.array.linearSearch(10)
        }
      }
    }
  }

  describe("Array::bubbleSort") {
    val f = fixture()

    it("Sorts the array elements") {
      f.array.bubbleSort()

      assertResult(f.sortedArray) (f.array)
    }
  }
}
