import game.Grid
import game.io.InputControl

object Main {
  def startGame(gridSize: Int) = {
    val grid  = new Grid(gridSize)
    val input = new InputControl(grid)
  }

  def main(args: Array[String]): Unit = args match {
    case Array(gridSize) => startGame(util.Try(gridSize.toInt).getOrElse(4))
    case _               => startGame(4)
  }
}
