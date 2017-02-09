package player
import puzzel.Puzzel
import scala.collection.mutable.ListBuffer

class Player (algorithmType: String) {
    var Algorithm = algorithmType
    var closedMoveList: List[Int] = List() // start empty
    var boardStates = ListBuffer(Array.ofDim[Int](3,3))

    def PrintGameState () = {
        for( k <- 0 until boardStates.size)  {
            for(
                i <- 0 until 3;
                j <- 0 until 3
            )  {
                    print (boardStates(k)(i)(j))
                    if (j == 2) println()
                }
            println()
        }
    }

    def BreadthFirstSearch() = {
        boardStates += Puzzel.TileBoard.map(_.clone)
        println("========")
        PrintGameState()
        println("========")
        var zeroTuple = FindZero(Puzzel.TileBoard)
        var zeroNumberedPosition = ConvertCord(zeroTuple(0)._1, zeroTuple(0)._2)
        var neighbors = validneighbors(zeroTuple(0)._1, zeroTuple(0)._2)
        closedMoveList = neighbors.map(x => ConvertCord(x(0), x(1)))
        closedMoveList = closedMoveList.filter(_ > 0)

    }

    /* Note these aren't sanity checked! */
    private def validneighbors(xCord: Int, yCord: Int): List[List[Int]] = 
        List(
            List(xCord - 1, yCord), List(xCord + 1, yCord),
            List(xCord, yCord - 1), List(xCord, yCord + 1)
        )
    

    private def FindZero(tmpTileBoard: Array[Array[Int]]) = 
        for {
            i <- 0 until tmpTileBoard.size
            j <- 0 until tmpTileBoard(i).size
            if tmpTileBoard(i)(j) == 0
          } yield (i, j)
    
    private def ConvertCord (xCord: Int, yCord: Int): Int = {
        var xtmpList: List[Int] = List()
        var ytmpList: List[Int] = List()
        yCord match {
            case 0 => ytmpList = List(1, 4, 7)
            case 1 => ytmpList = List(2, 5, 8)
            case 2 => ytmpList = List(3, 6, 9)
            case _ => return 0
        }
        xCord match {
            case 0 => xtmpList = List(1, 2, 3)
            case 1 => xtmpList = List(4, 5, 6)
            case 2 => xtmpList = List(7, 8, 9)
            case _ => return 0
        }

        (xtmpList intersect ytmpList)(0) // Return converted coordinate
        
    }
}