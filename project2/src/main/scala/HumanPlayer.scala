package com

import com.Discs.Disc
import com.GridStates.GameInProgress

import scala.io.StdIn

/**
 * Player instance that request changes from console (= from a human).
 * 
 * @constructor Creates a new Human Player instance.
 * @param name Name of player.
 * @param disc Disc type player plays with.
 */
class HumanPlayer(val name:String, val disc:Disc) extends Player {
  
  def next(grid: Grid): Int = {
     if (grid.state != GameInProgress) {
    	 throw new IllegalStateException("Game needs to be in progress!")
        }
     var choice:Int = 0
     do {
        printf("Column choice (0-"+(grid.NumOfCol-1)+") : ")
        try{
        	choice = StdIn.readInt()
        } catch {
            case e: Exception => println("Error not a valid number")
            next(grid)
        }
        
     } while(grid.dropPossible(choice) == false)
     choice
  }

}