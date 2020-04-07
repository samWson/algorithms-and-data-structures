package samwson

import org.scalatest.funspec.AnyFunSpec
import scala.collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls

class SetSpec extends AnyFunSpec {

  def fixture() = new {
    val set = samwson.Set(
      Array(ArrayBuffer(1, 2, 3))
    )
  }

  describe("Set::insert") {
    describe("The value is not in the set") {
      val f = fixture()

      it("Adds the value to the set") {
        f.set.insert(4)

        assertResult(true) {
          f.set.sort()
          f.set.contains(4)
        }
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
}
