import scala.collection.mutable.ArrayBuffer
import scala.io.Source

val program = Source
  .fromFile("input.txt")
  .mkString
  .split(',')
  .map(_.trim.toInt)

def run(program: Seq[Int], arg1: Int, arg2: Int): Int = {
  val state = ArrayBuffer(program: _*)
  var position = 0

  state.update(1, arg1)
  state.update(2, arg2)

  def exec(operation: (Int, Int) => Int): Unit =
    state.update(
      state(position + 3),
      operation(state(state(position + 1)), state(state(position + 2)))
    )

  while (position < state.size) {
    state(position) match {
      case 1  => exec((x, y) => x + y)
      case 2  => exec((x, y) => x * y)
      case 99 => position = state.size
      case op: Int =>
        throw new IllegalArgumentException(s"undefined opcode $op")
    }
    position += 4
  }

  state(0)
}
