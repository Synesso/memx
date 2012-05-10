package net.badgerhunt.memx

import scala.Char

case class Crossword(width: Int, height: Int, words: Set[Word] = Set.empty, letters: Map[(Int, Int), Char] = Map.empty) {

  lazy val gridString = {
    (0 until height).map{yy =>
      (0 until width).map{xx =>
        val c = letters(xx, yy)
        if (c == 0) "。" else c
      }.mkString + "\n"
    }
  }.mkString

  def placementsFor(text: String) = {
    (0 to height - text.length).flatMap {
      y => (0 until width).map {
        x => Placement(x, y, Vertical)
      }
    } ++
      (0 until height).flatMap {
        y => (0 to width - text.length).map {
          x => Placement(x, y, Horizontal)
        }
      }
  }

  def place(word: Word) = {
    if (word.letters.forall { l =>
      l.x > 0 && l.x < width &&
        l.y > 0 && l.y < height &&
        (!letters.contains(l.x, l.y) || letters(l.x, l.y) == l.char)
    }) {
      this.copy(
        words = words + word,
        letters = word.letters.foldLeft(letters) {(acc, l) => acc.updated((l.x, l.y), l.char)}
      )
    } else this
  }

  def letter(x: Int, y: Int) = letters(x, y)
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
  val letters = text.zipWithIndex.foldLeft(Seq.empty[Letter]) {
    case (seq, (c, i)) =>
      val (x, y) = applyOffset(i)
      Letter(c, x, y) +: seq
  }
}

case class Letter(char: Char, x: Int, y: Int)