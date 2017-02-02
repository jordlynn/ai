package puzzel

import Math.{sqrt, pow}

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

    /* SanityCheck() just makes sure the user made a valid choice */
    private def SanityCheck (tileChoice: Int): Double = {
        var xVal: Int = 0
        var yVal: Int = 0

        var dxVal: Int = 0
        var dyVal: Int = 0

        var zeroTuple = searchArrays(0) // Get the zero coord.
        xVal = zeroTuple(0)._1
        yVal = zeroTuple(0)._2

        tileChoice match {
            case 1 => {
                dxVal = 0
                dyVal = 0
            }
            case 2 => {
                dxVal = 0
                dyVal = 1
            }
            case 3 => {
                dxVal = 0
                dyVal = 2
            }
            case 4 => {
                dxVal = 1
                dyVal = 0
            }
            case 5 => {
                dxVal = 1
                dyVal = 1
            }
            case 6 => {
                dxVal = 1
                dyVal = 2
            }
            case 7 => {
                dxVal = 2
                dyVal = 0
            }
            case 8 => {
                dxVal = 2
                dyVal = 1
            }
            case 9 => {
                dxVal = 2
                dyVal = 2
            }
        }
        sqrt(pow((dxVal - xVal),2) + pow((dyVal - yVal),2)) // Calculate distance
    }

    private def searchArrays(lookup: Int ) =
        for {
            i <- 0 until TileBoard.size
            j <- 0 until TileBoard(i).size
            if TileBoard(i)(j) == lookup
      } yield (i, j)

    def MoveTile (tile: Int): Unit = {
        
        if (SanityCheck(tile) > 1) { // Check for valid tile move
                println("ERROR invalid move!")
                return 
            } 
        val tmpCord = searchArrays(0) // Look for zero tile, our empty space
        
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
            if (TileBoard(i)(j) == 0 ) print(" ")
            else print(TileBoard(i)(j))
            if(j == 2) println()
        }
    }
}
