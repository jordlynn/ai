package player
import puzzel.Puzzel
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

class Player (algorithmType: String) {
    var Algorithm = algorithmType
    var boardStates = ListBuffer(Array.ofDim[Int](3,3))
    
    var possibleMoves : List[List[Int]] = List()

    boardStates.remove(0)
    boardStates += Puzzel.TileBoard.map(_.clone)
    def PrintState (board: ListBuffer[Array[Array[Int]]]) = {
        for( k <- 0 until board.size)  {
            for(
                i <- 0 until 3;
                j <- 0 until 3
            )  {
                    print (board(k)(i)(j))
                    if (j == 2) println()
                }
            println()
        }
    }

    def PrintaState (board: Array[Array[Int]]) = {
            for(
                i <- 0 until 3;
                j <- 0 until 3
            )  {
                    print (board(i)(j))
                    if (j == 2) println()
                }
            println()
    }

    def BreadthFirstSearch(boardToSolve: Set[Array[Array[Int]]], moves: Seq[Int]): Seq[Int]  = {
        if(boardToSolve.isEmpty){
            println("uh oh")
        }
        val zeroTuple = FindZero(boardToSolve.head)
        val zeroPosition = ConvertCord(zeroTuple(0)._1, zeroTuple(0)._2)
        val neighbors = validneighbors(zeroTuple(0)._1, zeroTuple(0)._2)

        var moveList = neighbors.map(x => ConvertCord(x(0), x(1)))
        moveList = moveList.filter(_ > 0)
        moveList.map(print(_))
        println()
        var childrenList = moveList.map(Puzzel.FindChildren(_))
        //childrenList.map(PrintaState(_))
        childrenList.map(
            x => if(Puzzel.Solution.deep == x.deep) { 
                return moves :+ moveList(childrenList.indexOf(x))
            }
        )
        moves :+ moveList.head
        Puzzel.MoveTile(moveList.head)
        BreadthFirstSearch(boardToSolve.tail ++ childrenList.toSet, moves :+ moveList.head )
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