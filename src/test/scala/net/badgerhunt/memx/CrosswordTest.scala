package net.badgerhunt.memx

import org.specs2.mutable.Specification

class CrosswordTest extends Specification {

  "A crossword wider than it is high" should {
    val crossword = Crossword(4, 3)

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
    val crossword = Crossword(3, 4)

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

      updated.letters(3, 8) must beEqualTo('も')
      updated.letters(4, 8) must beEqualTo('ち')
      updated.letters(5, 8) must beEqualTo('ろ')
      updated.letters(6, 8) must beEqualTo('ん')
      updated.letters(4, 5) must beEqualTo('い')
      updated.letters(4, 6) must beEqualTo('り')
      updated.letters(4, 7) must beEqualTo('く')
    }

    "deny a placement that overwrites an existing letter with a different one" in {
      val updated = crossword.place(Word("もちろん", "of course", Placement(3, 3, Horizontal)))
      val result = updated.place(Word("ことば", "word", Placement(4, 2, Vertical)))
      result must beEqualTo(updated)
    }

    "deny a placement that breaches the left hand boundary" in {
      crossword.place(Word("むら", "village", Placement(-1, 2, Horizontal))) must beEqualTo(crossword)
    }

    "deny a placement that breaches the right hand boundary" in {
      crossword.place(Word("むらさき", "purple", Placement(7, 2, Horizontal))) must beEqualTo(crossword)
    }

    "deny a placement that breaches the top boundary" in {
      crossword.place(Word("なす", "eggplant", Placement(4, -1, Vertical))) must beEqualTo(crossword)
    }

    "deny a placement that breaches the bottom boundary" in {
      crossword.place(Word("げんきん", "cash", Placement(3, 8, Vertical))) must beEqualTo(crossword)
    }

    "deny a placement that breaches the top and bottom boundaries" in {
      crossword.place(Word("のどがかわきませんでした", "was not thirsty", Placement(3, -1, Vertical))) must beEqualTo(crossword)
    }

    "deny a placement that breaches the left and right hand boundaries" in {
      crossword.place(Word("おなかがすきませんでした", "was not hungry", Placement(-1, 1, Horizontal))) must beEqualTo(crossword)
    }

    "have a printable gridString for debugging" in {
      val string = crossword
        .place(Word("にかわ", "glue", Placement(5,6,Vertical)))
        .place(Word("かいわ", "conversation", Placement(3,8,Horizontal))).gridString
      string mustEqual(
        """
          |。。。。。。。。。。
          |。。。。。。。。。。
          |。。。。。。。。。。
          |。。。。。。。。。。
          |。。。。。。。。。。
          |。。。。。。。。。。
          |。。。。。に。。。。
          |。。。。。か。。。。
          |。。。かいわ。。。。
          |。。。。。。。。。。
        """.stripMargin.trim)
    }

  }

  "A crossword with an existing placement" should {
    val crossword = Crossword(12, 12).place(Word("かいわ", "conversation", Placement(3, 8, Horizontal)))

    "find zero placements for words that do no intersect (even if there is free space)" in {
      crossword.placementsFor("うなぎ") must beEmpty
    }

    "find a placement for words that intersect" in {
      crossword.placementsFor("おもしろい") must contain(Placement(4, 4, Vertical)).only
    }

    "find multiple placements for words that intersect multiple times" in {
      crossword.placementsFor("にかわ") must contain(
        Placement(3, 7, Vertical), Placement(5, 6, Vertical)
      ).only
    }

    "find placements that report auxiliary words by extending existing words" in {
      crossword.placementsFor("わかり") must contain(Placement(5, 8, Horizontal, Set("かいわかり")))
    }

    "find placements that report auxiliary words by prefixing existing words" in {
      crossword.placementsFor("しずか") must contain(Placement(1, 8, Horizontal, Set("しずかいわ")))
    }

    "not find placements that are substrings of existing words" in {
      crossword.placementsFor("かい") must not contain(Placement(3, 8, Horizontal, Set("かいわ")))
    }

    /*

      "find placements that report auxiliary words (ie new words created as a result of extending existing letters)" in {

      }

        "find zero placements for words that intersect but breach the left boundary" in {

        }
        "find zero placements for words that intersect but breach the right boundary" in {

        }
        "find zero placements for words that intersect but breach the top boundary" in {

        }
        "find zero placements for words that intersect but breach the bottom boundary" in {

        }
    */
  }
}
