package net.badgerhunt.memx

import org.specs2.mutable.Specification

class CrosswordTest extends Specification {

  "A crossword wider than it is high" should {
    val crossword = Crossword(4,3)

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
    val crossword = Crossword(3,4)

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

  "A crossword" should {
    val crossword = Crossword(10, 10)

    "allow a placement to be made" in {
      val word = Word("パチンコ", "pachinko", Placement(3, 3, Horizontal))
      val updated = crossword.place(word)
      updated.words must contain(word).only
    }

    "allow a subsequent placement to be made" in {
      val updated = crossword.place(
        Word("もちろん", "of course", Placement(3, 8, Horizontal))).place(
        Word("いりくち", "entrance", Placement(4, 5, Vertical)))
      updated.words must contain(
        Word("もちろん", "of course", Placement(3, 8, Horizontal)),
        Word("いりくち", "entrance", Placement(4, 5, Vertical))).only

      println(updated.gridString)

      updated.letter(3, 8) must beEqualTo('も')
      updated.letter(4, 8) must beEqualTo('ち')
      updated.letter(5, 8) must beEqualTo('ろ')
      updated.letter(6, 8) must beEqualTo('ん')
      updated.letter(4, 5) must beEqualTo('い')
      updated.letter(4, 6) must beEqualTo('り')
      updated.letter(4, 7) must beEqualTo('く')
    }

    "deny a placement that overwrites an existing letter with a different one" in {
      val updated = crossword.place(Word("もちろん", "of course", Placement(3, 3, Horizontal)))
      val result = updated.place(Word("ことば", "word", Placement(4, 2, Vertical)))
      result must beEqualTo(updated)
    }

    /*
        "allow a placement that intersects an existing word with the same letter" in {

        }

        "deny a placement that breaches the left hand boundary" in {

        }

        "deny a placement that breaches the right hand boundary" in {

        }

        "deny a placement that breaches the top boundary" in {

        }

        "deny a placement that breaches the bottom boundary" in {

        }

        "deny a placement that breaches the top and bottom boundaries" in {

        }

        "deny a placement that breaches the left and right hand boundaries" in {

        }
    */
  }
}
