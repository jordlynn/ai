class Puzzel(){
    var Solution = Array.ofDim[Int](3,3)
    var TileBoard = Array.ofDim[Int](3,3)
    var indexer = 1

    for (
        i <- 0 until 2;
        j <- 0 until 2
    ) yield {
        Solution(i)(j) = indexer
        indexer += 1 
    }


    def MoveTile (tile: Int) = {
        println("Moved")
    }
}
