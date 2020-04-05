package samwson

import org.scalatest.FunSpec
import scala.collection.mutable.ArrayBuffer

class SetSpec extends FunSpec {

  def fixture() = new {
    val set = samwson.Set(ArrayBuffer(1, 2, 3))
  }


  describe("Set::insert") {
    describe("The value is not in the set") {
      val f = fixture()

      it("Adds the value to the set") {
        f.set.insert(4)

        assertResult(true) (f.set.search(4))
        assertResult(4) (f.set.length)
      }
    }

    describe("The value is in the set") {
      val f = fixture()

      it("Does not allow duplicate values in the set") {
        val lengthBeforeInsertion = f.set.length()

        f.set.insert(3)

        assertResult(true) (f.set.length == lengthBeforeInsertion)
      }
    }
  }

  describe("Set::delete") {
    describe("The value is in the set") {
      val f = fixture()

      it("Removes the value from the set") {
        val lengthBeforeDeletion = f.set.length()

        f.set.delete(2)

        assertResult(false) (f.set.search(2))
        assertResult(true) (f.set.length() == lengthBeforeDeletion - 1)
      }
    }

    describe("The value is not in the set") {
      val f = fixture()

      it("Does nothing") {
        val lengthBeforeDeletion = f.set.length()

        f.set.delete(4)

        assertResult(true) (f.set.length() == lengthBeforeDeletion)
      }
    }
  }

  describe("Set::read") {
    describe("The read index is inside the set bounds") {
      val f = fixture()

      it("Returns the value at the index") {
        assertResult(2) (f.set.read(1))
      }
    }

    describe("The read index is out of the set bounds") {
      val f = fixture()

      it("Raises an exception") {
        assertThrows[IndexOutOfBoundsException] (f.set.read(3))
      }
    }
  }

  describe("Set::search") {
    describe("The searched value is in the set") {
      val f = fixture()

      it("Returns true") {
        assertResult(true) (f.set.search(2))
      }
    }

    describe("The searched value is not in the set") {
      val f = fixture()

      it("Returns false") {
        assertResult(false) (f.set.search(9))
      }
    }
  }
}
