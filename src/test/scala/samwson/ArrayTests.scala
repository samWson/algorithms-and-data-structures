package samwson

import org.scalatest.FunSpec
import scala.collection.mutable.ArrayBuffer

class ArraySpec extends FunSpec {

  def fixture() = new {
    val array = samwson.Array(ArrayBuffer(1, 8, 4, 5, 3, 9))
    val sortedArray = samwson.Array(ArrayBuffer(1, 3, 4, 5, 8, 9))
  }

  describe("Array::insert") {
    val f = fixture()

    it("Adds the value to the array") {
      val lengthBeforeInsertion = f.array.length()

      f.array.insert(2)

      assertResult(Some(3)) {
        f.array.bubbleSort()
        f.array.linearSearch(4)
      }

      assertResult(lengthBeforeInsertion + 1) (f.array.length())
    }
  }

  describe("Array::delete") {
    val f = fixture()

    it("Removes the value and returns the value at the index") {
      val lengthBeforeDeletion = f.array.length()

      val deletedValue = f.array.delete(2)

      assertResult(None) {
        f.array.bubbleSort()
        f.array.linearSearch(4)
      }

      assertResult(4) (deletedValue)
      assertResult(lengthBeforeDeletion - 1) (f.array.length())
    }
  }

  describe("Array::read") {
    val f = fixture()

    it("Returns the value at the read index") {
      assertResult(4) (f.array.read(2))
    }
  }

  describe("Array::linearSearch") {
    val f = fixture()

    describe("The value is in the array") {
      it("Returns the index of the first matching value") {
        assertResult(Some(2)) {
          f.array.bubbleSort()
          f.array.linearSearch(4)
        }
      }
    }

    describe("The value is not in the array") {
      it("Returns None") {
        assertResult(None) (f.array.linearSearch(2))
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
