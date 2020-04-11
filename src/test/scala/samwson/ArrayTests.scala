package samwson

import org.scalatest.funspec.AnyFunSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

class ArraySpec extends AnyFunSpec {

  def fixture() = new {
    val array = samwson.Array(ArrayBuffer(1, 8, 4, 5, 3, 9, 7, 2, 6))
    val sortedArray = samwson.Array(ArrayBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  def intersectionFixture() = new {
    val arrayWithCommonElements = ArrayBuffer(1, 17, 9, 10, 5, 14, 2)
    val arrayWithoutCommonElements = ArrayBuffer(17, 10, 14)
    val expectedIntersection = ArrayBuffer(1, 9, 5, 2)
  }

  describe("Array::insert") {
    val f = fixture()

    it("Adds the value to the array") {
      val lengthBeforeInsertion = f.array.length()

      f.array.insert(10)

      assertResult(true) {
        f.array.sort()
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
        f.array.sort()
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
          f.array.sort()
          f.array.contains(4)
        }
      }
    }

    describe("The value is not in the array") {
      it("Returns false") {
        assertResult(false) {
          f.array.sort()
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
          f.array.sort()
          f.array.linearSearch(4)
        }
      }
    }

    describe("The value is not in the array") {
      it("Returns None") {
        assertResult(None) {
          f.array.sort()
          f.array.linearSearch(10)
        }
      }
    }
  }

  describe("Array::hasDuplicateValue") {
    describe("The array has a duplicate value") {
      val f = fixture()
      f.array.insert(7)

      it("Returns true") {
        assertResult(true) (f.array.hasDuplicateValue())
      }
    }

    describe("The array does not have any duplicate values") {
      val f = fixture()

      it("Returns false") {
        assertResult(false) (f.array.hasDuplicateValue())
      }
    }
  }

  describe("Array::intersection") {
    describe("The two arrays have common elements") {
      val f = fixture()
      val g = intersectionFixture()

      it("Returns the intersection of the arrays") {
        assertResult(g.expectedIntersection.sortInPlace) {
          f.array.intersection(g.arrayWithCommonElements).sortInPlace
        }
      }
    }

    describe("The two arrays have no elements in common") {
      val f = fixture()
      val g = intersectionFixture()

      it("Returns an empty array") {
        assertResult(ArrayBuffer()) (f.array.intersection(g.arrayWithoutCommonElements))
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

  describe("Array::selectionSort") {
    val f = fixture()

    it("Sorts the array elements") {
      f.array.selectionSort()

      assertResult(f.sortedArray) (f.array)
    }
  }

  describe("Array::insertionSort") {
    val f = fixture()

    it("Sorts the array elements") {
      f.array.insertionSort()

      assertResult(f.sortedArray) (f.array)
    }
  }

  describe("Array::binarySearch") {
    val f = fixture()
    f.array.sort()

    describe("The searched element is in the lower half of the array") {
      it("Returns the index of the searched element") {
        assertResult(Some(2)) (f.array.binarySearch(3))
      }
    }

    describe("The searched element is the upper half of the array") {
      it("Returns the index of the searched element") {
        assertResult(Some(7)) (f.array.binarySearch(8))
      }
    }

    describe("The value is not in the array") {
      it("Returns None") {
        assertResult(None) (f.array.binarySearch(10))
      }
    }
  }
}
