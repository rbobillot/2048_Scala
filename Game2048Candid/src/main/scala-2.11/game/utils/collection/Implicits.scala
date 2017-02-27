package game.utils.collection

object Implicits {
  type Row    = Array[Int]
  type Matrix = Array[Row]
  type Coords = Array[(Int,Int)]

  implicit class Flat[T](arr: Array[T]) {
    def getRandElement = arr(util.Random.nextInt(arr.length))
  }

  implicit class Row2048(arr: Row) {
    def cleanAndPad(implicit rowSize: Int = 4): Row =
      arr.filterNot(_ == 0).padTo(rowSize, 0)
  }

  implicit class Matrix2048(matrix:Matrix) {
    def getFreeCells:Coords = for {
      (_,x) <- matrix.zipWithIndex
      (_,y) <- matrix(x).zipWithIndex if (matrix(x)(y) == 0)
    } yield (x,y)

    def isMergeable =
      matrix.flatten.contains(0) ||
      matrix.exists(line => (line zip line.tail).exists{case (x,y) => x == y})

    def isIdentical(grid:Matrix) = {
      def check:Array[Coords] => Boolean = _.forall(_.forall{case (x,y) => x == y})

      val (t1,t2) = (grid.transpose, matrix.transpose)
      val zip1    = grid.zip(matrix).map{case (x,y) => x zip y}
      val zip2    = t1.zip(t2).map{case (x,y) => x zip y}

      check(zip1) && check(zip2)
    }

    def notIdentical(grid: Matrix) = ! isIdentical(grid)
  }
}
