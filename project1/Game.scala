import puzzel.Puzzel
import player.Player
import scala.io.StdIn.{readLine,readInt}


object Game {
    
    def main(args: Array[String]): Unit = {

        var compPlayer = new Player("BFS")

        while(!Puzzel.isSolved()) { 
        	println("Current position:")
        	Puzzel.PrintBoard()

        	print("Your move: ")
            compPlayer.BreadthFirstSearch(Set(Puzzel.TileBoard))
            var tmp = readInt()
            
            Puzzel.MoveTile(tmp)

        }
        println()
        Puzzel.PrintBoard()
        println("Solved!")
    }
}