package game.io

import game.Grid

object OutputControl {
  def hideKeyboardOutput = System.console.readPassword

  def stopGame(status:String):Unit = {
    status match {
      case "lose" => println(" "*12 + "\u001b[91mYOU LOSE\u001b[0m !")
      case "win"  => println(" "*12 + "\u001b[92m YOU WIN\u001b[0m !")
    }
    sys exit 0
  }

  def cleanCell(num:Int, i:Int):String = num match {
    case 0 => " "
    case n => if (0 == i % 2) n.toString else n.toString.map(_ => ' ')
  }

  def colorCell(num:Int, i:Int):String = num match {
    case 0    => "\u001b[1;30;48;5;236m   " + cleanCell(num,i) + "   \u001b[0m"
    case 2    => "\u001b[1;30;48;5;231m   " + cleanCell(num,i) + "   \u001b[0m"
    case 4    => "\u001b[1;30;48;5;229m   " + cleanCell(num,i) + "   \u001b[0m"
    case 8    => "\u001b[1;37;48;5;215m   " + cleanCell(num,i) + "   \u001b[0m"
    case 16   => "\u001b[1;37;48;5;208m   " + cleanCell(num,i) + "  \u001b[0m"
    case 32   => "\u001b[1;37;48;5;202m   " + cleanCell(num,i) + "  \u001b[0m"
    case 64   => "\u001b[1;37;48;5;196m   " + cleanCell(num,i) + "  \u001b[0m"
    case 128  => "\u001b[1;30;48;5;222m  "  + cleanCell(num,i) + "  \u001b[0m"
    case 256  => "\u001b[1;30;48;5;221m  "  + cleanCell(num,i) + "  \u001b[0m"
    case 512  => "\u001b[1;30;48;5;220m  "  + cleanCell(num,i) + "  \u001b[0m"
    case 1024 => "\u001b[1;30;48;5;226m  "  + cleanCell(num,i) + " \u001b[0m"
    case 2048 => "\u001b[1;30;48;5;11m  "   + cleanCell(num,i) + " \u001b[0m"
  }

  def printAsciiGrid(grid:Grid, row:Int = 0, index:Int = 1):Unit =
    if (0 == index % grid.gridSize) {
      println(if (row != 1) "" else (" "*grid.gridSize*10) + "\u001b[96mScore\u001b[0m: " + grid.score)
      printAsciiGrid(grid, row+1, index+1)
    }
    else if (row < grid.gridSize) {
      println(grid.tiles(row).map(num => colorCell(num, index)).mkString("  "))
      printAsciiGrid(grid, row, index+1)
    }

  def printKeyInformations():Unit = {
    println(" "*3 + "Press <ENTER>  to exit game")
    println(" "*3 + "Press <ESCAPE> to exit game\n")
    println(" "*3 + "Press <R> to reset game\n")
  }

  def showGrid(grid:Grid):Unit = {
    println("\u001b[1J\u001b[1;1H")
    printKeyInformations()
    printAsciiGrid(grid)
    if (grid.isLocked) stopGame("lose")
    if (grid.isVictory) stopGame("win")
  }
}
