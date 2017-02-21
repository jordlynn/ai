import puzzel.Puzzel
import player.Player
import scala.io.StdIn.{readLine,readInt}


object Game {
    
    def main(args: Array[String]): Unit = {

        var compPlayer = new Player("BFS")
        val SolvedSeq = compPlayer.BreadthFirstSearch(Set(Puzzel.TileBoard), Seq.empty)

/*        while(!Puzzel.isSolved()) { 
        	println("Current position:")
        	Puzzel.PrintBoard()

        	print("Your move: ")
            
            var tmp = readInt()
            
            //Puzzel.MoveTile(tmp)

        }*/
        print("Sequence to solve: ")
        SolvedSeq.map(print(_))

        SolvedSeq.map(Puzzel.MoveTile(_))
        
        println("\nSolved!")
        println()
        Puzzel.PrintBoard()
    }
}