package game

import puzzel.Puzzel
import player.Player
import scala.io.StdIn.{readLine,readInt}


object Game {
    
    def main(args: Array[String]): Unit = {
        var compPlayer = new Player("BFS")

        while(!Puzzel.isSolved()) { 
        	println("Current position:")
        	Puzzel.PrintBoard()
            compPlayer.BreadthFirstSearch()
        	print("Your move: ")
            var input = readInt()
            if(input == 666){
                compPlayer.PrintGameState()
            } else {
                Puzzel.MoveTile(input)
            }

        }
        Puzzel.PrintBoard()
        println("Solved!")
    }
}