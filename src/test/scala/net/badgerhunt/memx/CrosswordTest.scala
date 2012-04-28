package net.badgerhunt.memx

import org.specs2.mutable.Specification

class CrosswordTest extends Specification {

  "A crossword wider than it is high" should {
    val crossword = new Crossword(4,3)

    "find no placements for words that are too large" in {
      crossword.placementsFor("しゅくだい") must beEmpty
    }
    "find only horizontal placements for words that are too large vertically" in {
      crossword.placementsFor("あんぜん") must contain(
        Placement(0, 0, Horizontal),
        Placement(0, 1, Horizontal),
        Placement(0, 2, Horizontal)
      ).only
    }

    "find horizontal and vertical placements for words that will fit" in {
      crossword.placementsFor("すごい") must contain(
        Placement(0, 0, Horizontal),
        Placement(1, 0, Horizontal),
        Placement(0, 1, Horizontal),
        Placement(1, 1, Horizontal),
        Placement(0, 2, Horizontal),
        Placement(1, 2, Horizontal),
        Placement(0, 0, Vertical),
        Placement(1, 0, Vertical),
        Placement(2, 0, Vertical),
        Placement(3, 0, Vertical)
      ).only
    }
  }

  "A crossword higher than it is wide" should {
    val crossword = new Crossword(3,4)

    "find no placements for words that are too large" in {
      crossword.placementsFor("しゅくだい") must beEmpty
    }

    "find only vertical placements for words that are too large horizontally" in {
      crossword.placementsFor("あんぜん") must contain(
        Placement(0, 0, Vertical),
        Placement(1, 0, Vertical),
        Placement(2, 0, Vertical)
      ).only
    }

    "find horizontal and vertical placements for words that will fit" in {
      crossword.placementsFor("すごい") must contain(
        Placement(0, 0, Vertical),
        Placement(0, 1, Vertical),
        Placement(1, 0, Vertical),
        Placement(1, 1, Vertical),
        Placement(2, 0, Vertical),
        Placement(2, 1, Vertical),
        Placement(0, 0, Horizontal),
        Placement(0, 1, Horizontal),
        Placement(0, 2, Horizontal),
        Placement(0, 3, Horizontal)
      ).only
    }
  }
}
