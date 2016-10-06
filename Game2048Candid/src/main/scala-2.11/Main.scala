import game.Grid
import game.io.InputControl

object Main {
  def main(args: Array[String]): Unit = {
    val grid  = new Grid
    val input = new InputControl(grid)
  }
}
