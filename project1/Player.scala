package player
import puzzel.Puzzel

class Player (algorithmType: String) {
    var Algorithm = algorithmType
    var closedList: List[Int] = List() // start empty

    def BreadthFirstSearch() = {
        closedList = List()
        var zeroTuple = FindZero(Puzzel.TileBoard)
        var zeroNumberedPosition = ConvertCord(zeroTuple(0)._1, zeroTuple(0)._2)
        var neighbors = validneighbors(zeroNumberedPosition)
        closedList = closedList ++ neighbors
        println("List " + closedList)
    }

    /* Note these aren't sanity checked! */
    private def validneighbors(zeroPosition: Int): List[Int] = 
        List(zeroPosition + 1, zeroPosition - 1, zeroPosition + 3, zeroPosition - 3 )

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
        }
        xCord match {
            case 0 => xtmpList = List(1, 2, 3)
            case 1 => xtmpList = List(4, 5, 6)
            case 2 => xtmpList = List(7, 8, 9)
        }
        (xtmpList intersect ytmpList)(0) // Return converted coordinate
        
    }
}