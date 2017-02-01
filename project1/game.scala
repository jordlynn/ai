import puzzel.Puzzel
import scala.io.StdIn.{readLine,readInt}


object Game {
    def main(args: Array[String]): Unit = {
       Puzzel.PrintBoard()
        while(!Puzzel.isSolved()) {
            var input = readInt()
            Puzzel.MoveTile(input)
            Puzzel.PrintBoard()
        }

    }
}