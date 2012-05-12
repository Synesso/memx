package net.badgerhunt.memx

import scala.Char

case class Crossword(width: Int, height: Int, words: Set[Word] = Set.empty, letters: Map[(Int, Int), Char] = Map.empty) {

  def placementsFor(text: String) = {
    val placementsGivenDimensions =
      (0 to height - text.length).flatMap {y => (0 until width).map {x => Placement(x, y, Vertical)}} ++
      (0 until height).flatMap {y => (0 to width - text.length).map {x => Placement(x, y, Horizontal)}}

    // there are currently no words; or
    // this word placement intersects at least one existing letter
    val firstOrIntersectingPlacements = placementsGivenDimensions.filter{placement =>
      words.isEmpty ||
      Word(text, "", placement).intersects(letters)
    }

    // any extra words formed by extending forward or backwards are attached (may not be valid words)
    val validPlacementsWithTheirExtendedAuxiliaryWords = firstOrIntersectingPlacements.map{placement =>
      // search backwards and forwards to find if the complete word is different
      def textPrior(from: String): String = {
        letters.get(placement.back(from.size + 1).xy).map(char => textPrior(char + from)).getOrElse(from)
      }

      def textAfter(from: String): String = {
        letters.get(placement.forward(from.size).xy).map(char => textAfter(from + char)).getOrElse(from)
      }

      val expandedWord = textPrior("") + textAfter(text)
      if (expandedWord == text) placement else placement.copy(aux = Set(expandedWord))
    }

    // any extra words formed by joining perpendicular to any new letters are attached (may not be valid words)
    val validPlacementsWithTheirAuxiliaryWords = validPlacementsWithTheirExtendedAuxiliaryWords.map{placement =>
      def textPrior(from: String, placement: Placement): String = {
        letters.get(placement.back(from.size + 1).xy).map(char => textPrior(char + from, placement)).getOrElse(from)
      }

      def textAfter(from: String, placement: Placement): String = {
        letters.get(placement.forward(from.size).xy).map(char => textAfter(from + char, placement)).getOrElse(from)
      }

      val auxWords: Set[String] = (0 until text.size).toSet.flatMap{i: Int =>
        val ithCharString = "" + text.charAt(i)
        val intersectionAtIthChar = placement.forward(i).turn
        val isIntersectionWithExistingWord = letters.get(intersectionAtIthChar.xy) == Some(text.charAt(i))
        if (isIntersectionWithExistingWord) None
        else {
          val expandedWord = textPrior("", intersectionAtIthChar) + textAfter(ithCharString, intersectionAtIthChar)
          if (expandedWord == ithCharString) None else Some(expandedWord)
        }
      }

      placement.copy(aux = placement.aux ++ auxWords)
    }

    validPlacementsWithTheirAuxiliaryWords
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

  lazy val gridString = {
    (0 until height).map{yy =>
      (0 until width).map{xx =>
        letters.getOrElse((xx, yy), "。")
      }.mkString + "\n"
    }
  }.mkString.trim
}

sealed trait Orientation
case object Horizontal extends Orientation
case object Vertical extends Orientation

case class Placement(x: Int, y: Int, orientation: Orientation, aux: Set[String] = Set.empty) {
  lazy val xy = (x, y)
  def back(i: Int) = if (orientation == Horizontal) this.copy(x = x - i) else this.copy(y = y - i)
  def forward(i: Int) = if (orientation == Horizontal) this.copy(x = x + i) else this.copy(y = y + i)
  def turn = if (orientation == Horizontal) this.copy(orientation = Vertical) else this.copy(orientation = Horizontal)
}

case class XY(x: Int, y: Int) {

}

case class Word(text: String, clue: String, placement: Placement) {

  def intersects(allLetters: Map[(Int, Int), Char]) = {
    letters.exists{l => allLetters.contains((l.x,l.y))} &&
    letters.exists{l => !allLetters.contains((l.x,l.y))} &&
    letters.forall{l =>
      allLetters.get((l.x,l.y)).map{char => char == l.char}.getOrElse(true)
    }
  }

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