package puzzel

object Puzzel {
    var Solution = Array.ofDim[Int](3,3)
    var TileBoard = Array.ofDim[Int](3,3)
    var indexer = 0

    for (
        i <- 0 until 3;
        j <- 0 until 3
    ) {
        Solution(i)(j) = indexer
        TileBoard(i)(j) = indexer
        indexer += 1 

    }

    private def searchArrays( ) =
        for {
        i <- 0 until TileBoard.size
        j <- 0 until TileBoard(i).size
        if TileBoard(i)(j) == 0
      } yield (i, j)

    def MoveTile (tile: Int) = {
        println("Moved")
        val tmpCord = searchArrays()
        println(tmpCord)
        
        tile match {
            case 1 => { TileBoard(tmpCord(0)._1)(tmpCord(0)._2) = TileBoard(0)(0)
                        TileBoard(0)(0) = 0 }
            case 2 => { TileBoard(tmpCord(0)._1)(tmpCord(0)._2) = TileBoard(0)(1)
                        TileBoard(0)(1) = 0 }
            case 3 => { TileBoard(tmpCord(0)._1)(tmpCord(0)._2) = TileBoard(0)(2)
                        TileBoard(0)(2) = 0 }

            case 4 => { TileBoard(tmpCord(0)._1)(tmpCord(0)._2) = TileBoard(1)(0)
                        TileBoard(1)(0) = 0 }
            case 5 => { TileBoard(tmpCord(0)._1)(tmpCord(0)._2) = TileBoard(1)(1)
                        TileBoard(1)(1) = 0 }
            case 6 => { TileBoard(tmpCord(0)._1)(tmpCord(0)._2) = TileBoard(1)(2)
                        TileBoard(1)(2) = 0 }

            case 7 => { TileBoard(tmpCord(0)._1)(tmpCord(0)._2) = TileBoard(1)(0)
                        TileBoard(1)(0) = 0 }
            case 8 => { TileBoard(tmpCord(0)._1)(tmpCord(0)._2) = TileBoard(1)(1)
                        TileBoard(1)(1) = 0 }
            case 9 => { TileBoard(tmpCord(0)._1)(tmpCord(0)._2) = TileBoard(1)(2)
                        TileBoard(1)(2) = 0 }
        }
    }

    def isSolved () : Boolean = {
        if (Solution.sameElements(TileBoard)) return true
        else return false
    }

    def PrintBoard () = {
        for (
            i <- 0 until 3;
            j <- 0 until 3
        ) yield {
            print(TileBoard(i)(j))
            if(j == 2) println()
        }
    }
}
