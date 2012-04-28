package net.badgerhunt.memx

class Crossword(width: Int, height: Int) {
  def placementsFor(text: String) = {
    (0 to height - text.length).flatMap{y => (0 until width).map{x => Placement(x, y, Vertical)}} ++
      (0 until height).flatMap{y => (0 to width - text.length).map{x => Placement(x, y, Horizontal)}}
  }
}

sealed trait Orientation
case object Horizontal extends Orientation
case object Vertical extends Orientation

case class Placement(x: Int, y: Int, orientation: Orientation)