package com

import com.Discs.{Player2, Player1}
import com.GridStates.{Full, GameInProgress, FourInALine}

object Main extends App {
    val NumOfCol = 7
    val NumOfRow = 6
    val treeDepth = 9

    val grid = Grid (NumOfCol, NumOfRow)
    //val playerOne = new HumanPlayer ("Player 1", Player1)
    val playerTwo = new HumanPlayer ("Player 2", Player2)
    //val playerTwo = new ComputerPlayer("Computer", Player2, treeDepth) with BetterEvaluation
    //val playerTwo = new ComputerPlayer("Computer", Player2, 8) with BetterEvaluation
    val playerOne = new ComputerPlayer("Computer1", Player1, 8) with BetterEvaluation
    

    draw (grid, playerOne, playerTwo)
    val finalGrid = playGame (playerOne, playerTwo, grid)

    finalGrid.state match {
        case FourInALine =>
            println ("Four in a row!")
            val winningPlayer = if (finalGrid.winningDisc == Some (playerOne.disc)) {
                playerOne.name
            } else {
                playerTwo.name
            }
            println (s"Player $winningPlayer won.")
        case Full =>
            println ("Draw, the board is full")
        case _ =>
            throw new IllegalStateException ("Oh no the board entered an illegal state!")
    }


    private def playGame (currentPlayer: Player, otherPlayer: Player, grid: Grid): Grid = {
        val next = currentPlayer.next (grid)
        val updatedGrid = grid.drop (next, currentPlayer.disc)

        draw (updatedGrid, playerOne, playerTwo)

        if (updatedGrid.state == GameInProgress) {
            playGame (otherPlayer, currentPlayer, updatedGrid)
        } else updatedGrid
    }

    private def draw(grid: Grid, playerOne: Player, playerTwo: Player): Unit = {
        println(s"""${playerOne.name}: ${playerOne.disc.asciiRepresentation}, ${playerTwo.name}: ${playerTwo.disc.asciiRepresentation}""")

        // Print board
        for (row <- 0 until NumOfRow) {
            for (col <- 0 until NumOfCol) {
                grid.value(col, row) match {
                    case Some(player) => print(player.asciiRepresentation)
                    case None => print("_")
                }
            }
            println()
        }
    }
}
