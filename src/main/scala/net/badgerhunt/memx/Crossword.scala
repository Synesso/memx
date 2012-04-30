package net.badgerhunt.memx

case class Crossword(width: Int, height: Int, words: Set[Word] = Set.empty) {

  private val grid = words.foldLeft(Array.ofDim[Char](width, height)) {(letters, word) =>
    word.letters.foldLeft(letters) { (acc, l) =>
      acc.updated(l.x, letters(l.x).updated(l.y, l.char))
    }
  }

//  println()
//  grid.foreach(l => println(l.map(c => if (c == 0) "." else c).foldLeft(""){_+_}))

  def placementsFor(text: String) = {
    (0 to height - text.length).flatMap{y => (0 until width).map{x => Placement(x, y, Vertical)}} ++
      (0 until height).flatMap{y => (0 to width - text.length).map{x => Placement(x, y, Horizontal)}}
  }
  def place(word: Word) = if (word.letters.forall(l => grid(l.x)(l.y) == l.char || grid(l.x)(l.y) == 0)) {
    this.copy(words = words + word)
  } else this
}

sealed trait Orientation
case object Horizontal extends Orientation
case object Vertical extends Orientation

case class Placement(x: Int, y: Int, orientation: Orientation)
case class Word(text: String, clue: String, placement: Placement) {
  private val applyOffset = placement.orientation match {
    case Horizontal => (i: Int) => (placement.x + i, placement.y)
    case Vertical => (i: Int) => (placement.x, placement.y + i)
  }
  val letters = text.zipWithIndex.foldLeft(Seq.empty[Letter]) { case (seq, (c, i)) =>
    val (x,y) = applyOffset(i)
    Letter(c, x, y) +: seq
  }
}
case class Letter(char: Char, x: Int, y: Int)