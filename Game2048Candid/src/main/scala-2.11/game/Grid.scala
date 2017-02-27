package game

import game.utils.collection.Implicits._
import io.OutputControl.showGrid

class Grid {
  var score:Int    = 0
  val tiles:Matrix = emptyGrid

  def emptyGrid:Matrix  = Array.fill(4)(Array.fill(4)(0))

  def isFull:Boolean    = !tiles.flatten.contains(0)
  def isLocked:Boolean  = !(tiles.isMergeable || tiles.transpose.isMergeable)
  def isVictory:Boolean = tiles.flatten.contains(2048)

  def updateGridFrom(matrix:Matrix):Unit =
    matrix.zipWithIndex.foreach{ case (row, index) => tiles.update(index, row) }

  def addTile:Grid = if (isFull) this else {
    val (x, y): (Int, Int) = tiles.getFreeCells.getRandElement
    val newElem: Int       = Array(2,2,2,2,2,4).getRandElement

    tiles.update(x, tiles(x).updated(y, newElem))
    this
  }

  def updateScoreAndTiles(tile: Int, tiles: List[Int]): List[Int] = {
    score  += tile*2
    tile*2 :: tiles
  }

  def mergeLeftDynamicLength(rw:Row): Row =
    rw.foldLeft(0 :: Nil){ (acc, e) => acc.head match {
      case `e` => 0 :: updateScoreAndTiles(e, acc drop 1)
      case  _  => e :: acc
    }}.reverse.toArray

  def mergeLeftBasicLength(row:Row): Row = row match {
    case Array(a,b,c,d) if a == b && c == d => score += a+b+c+d ; Array(a+b,c+d,0,0)
    case Array(a,b,c,d) if a == b           => score += a+b     ; Array(a+b,c,d,0)
    case Array(a,b,c,d) if b == c           => score += b+c     ; Array(a,b+c,d,0)
    case Array(a,b,c,d) if c == d           => score += c+d     ; Array(a,b,c+d,0)
    case _                                  => row
  }

  def mergeLeft(row:Row)(implicit size: Int = row.size): Row =
    (size match {
      case 4 => mergeLeftBasicLength(row.cleanAndPad)
      case _ => mergeLeftDynamicLength(row.cleanAndPad)
    }).cleanAndPad

  def mergeTiles(grid:Matrix):Matrix = grid map mergeLeft

  def move(direction:String): Unit = if (!isLocked) {
    val newCells = direction match {
      case "left"  => mergeTiles(tiles)
      case "right" => mergeTiles(tiles.map(_.reverse)).map(_.reverse)
      case "down"  => mergeTiles(tiles.reverse.transpose).transpose.reverse
      case "up"    => mergeTiles(tiles.transpose.reverse).reverse.transpose
    }
    if (!(newCells isIdentical tiles)) {
      updateGridFrom(newCells)
      updateGridWithNewTile()
    }
  }

  def updateGridWithNewTile():Unit = {
    showGrid(this)   ; addTile
    Thread.sleep(80) ; showGrid(this)
  }

  def init():Unit = {
    score = 0
    updateGridFrom(emptyGrid)
    showGrid(this.addTile.addTile)
  }

  init()
}
