package samwson

import org.scalatest.funspec.AnyFunSpec
import scala.language.reflectiveCalls

class StackSpec extends AnyFunSpec {
  def fixture() = new {
    val stack = samwson.Stack(
      samwson.Array(scala.collection.mutable.ArrayBuffer(4, 2, 5))
    )
  }

  describe("Stack::push") {
    val f = fixture()

    it("Puts new elements on top of the stack") {
      val sizeBeforeRead = f.stack.size()

      f.stack.push(8)

      assertResult(8) (f.stack.read())
      assertResult(sizeBeforeRead + 1) (f.stack.size())
    }
  }

  describe("Stack::read") {
    val f = fixture()

    it("Returns the top of the stack without removing it") {
      val sizeBeforeRead = f.stack.size()

      assertResult(5) (f.stack.read())
      assertResult(sizeBeforeRead) (f.stack.size())
    }
  }

  describe("Stack::pop") {
    val f = fixture()

    it("Removes and returns the top of the stack") {
      val sizeBeforeRead = f.stack.size()

      assertResult(5) (f.stack.pop())
      assertResult(sizeBeforeRead - 1) (f.stack.size())
      assertResult(2) (f.stack.read())
    }
  }
}
