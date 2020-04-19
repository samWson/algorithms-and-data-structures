package samwson

import org.scalatest.funspec.AnyFunSpec
import scala.language.reflectiveCalls

class QueueSpec extends AnyFunSpec {
  def fixture() = new {
    val queue = new samwson.Queue()
      .enqueue(8)
      .enqueue(4)
      .enqueue(6)
  }

  describe("Queue::enqueue") {
    val f = fixture()

    it("Adds elements to the end of the queue") {
      val lengthBeforeEnqueue = f.queue.length()

      f.queue.enqueue(2)

      assertResult(lengthBeforeEnqueue + 1) (f.queue.length())
    }
  }

  describe("Queue::dequeue") {
    val f = fixture()

    it("Removes elements from the front of the queue") {
      val lengthBeforeDequeue = f.queue.length()

      assertResult(8) (f.queue.dequeue())
      assertResult(lengthBeforeDequeue - 1) (f.queue.length())
      assertResult(4) (f.queue.read())
    }
  }

  describe("Queue::read") {
    val f = fixture()

    it("Returns the element at the front of the queue") {
      val lengthBeforeRead = f.queue.length()

      assertResult(8) (f.queue.read())
      assertResult(lengthBeforeRead) (f.queue.length())
    }
  }
}
