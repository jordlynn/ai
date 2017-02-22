import puzzel.Puzzel
import player.Player
import scala.io.StdIn.{readLine,readInt}


object Game {
    
    def main(args: Array[String]): Unit = {

        var compPlayerBFS = new Player("BFS")
        var compPlayerDFS = new Player("BFS")
        var compPlayerAstr = new Player("BFS")
        Puzzel.RandomizeTable()
        Puzzel.PrintBoard()
        val tmpTileBoard = Puzzel.TileBoard.map(_.clone)

        
        
    

/*        while(!Puzzel.isSolved()) { 
        	println("Current position:")
        	Puzzel.PrintBoard()

        	print("Your move: ")
            
            var tmp = readInt()
            
            //Puzzel.MoveTile(tmp)

        }*/
        val SolvedSeqBFS = compPlayerBFS.BreadthFirstSearch(Set(Puzzel.TileBoard), Seq.empty)
        Puzzel.TileBoard = tmpTileBoard
        print("Sequence to solve: ")
        SolvedSeqBFS.map(print(_))
        println(" in " + SolvedSeqBFS.size + " moves")
        println(" took " + compPlayerBFS.stepsTaken + " steps.")
        SolvedSeqBFS.map(Puzzel.MoveTile(_))

        val SolvedSeqDFS = compPlayerDFS.DepthFirstSearch(Set(Puzzel.TileBoard), Seq.empty) 
        Puzzel.TileBoard = tmpTileBoard
        print("Sequence to solve: ")
        SolvedSeqDFS.map(print(_))
        println(" in " + SolvedSeqDFS.size + " moves")
        println(" took " + compPlayerDFS.stepsTaken + " steps.")
        SolvedSeqDFS.map(Puzzel.MoveTile(_))

         val SolvedSeqAStr = compPlayerAstr.AstarSearch(Set(Puzzel.TileBoard), Seq.empty) 
        print("Sequence to solve: ")
        SolvedSeqAStr.map(print(_))
        println(" in " + SolvedSeqAStr.size + " moves")
        println(" took " + compPlayerAstr.stepsTaken + " steps.")
        SolvedSeqAStr.map(Puzzel.MoveTile(_))
        
        println("\nSolved!")
        println()
        Puzzel.PrintBoard()
    }
}