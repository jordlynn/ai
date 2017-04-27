package com

/**
 * Represents the different 'discs' that can be dropped in the board.
 */
object Discs {

  sealed abstract class Disc(val asciiRepresentation:Char)

  case object Player1 extends Disc('o')
  case object Player2 extends Disc('x')

}