package com.log4p

import scala.util.matching.Regex.Match
import scala.language.implicitConversions

/**
 * Implementation of a stemming algorithm for the Dutch language.
 *
 * The full specification on which this code is based can be found at:
 *        <a href="http://snowball.tartarus.org/algorithms/dutch/stemmer.html">http://snowball.tartarus.org/algorithms/dutch/stemmer.html</a>
 *
 * @author Peter Maas (pfmmaas [at] gmail [dot] com)
 */
object DutchStemmer {
  import Accents._	
  import com.log4p.Pipeline

  implicit def str2payload(s: String): Payload = Payload(s)

  private val iBetweenVowels = "([yaieouè]+)i([yaieouè]+)".r
  private val yAfterVowels = "([yaieouè]+)y".r

  private val duplicateEndingPattern = """.*(kk|dd|tt)$""".r
  private val endsWithIgButNotEig = """.*[^e]ig$|^ig$""".r
  private val endsWithIngOrEnd = """(.*)(ing|end)$""".r
  private val endsWithLijk = """(.*)(lijk)$""".r
  private val endsWithBar = """(.*)(bar)$""".r
  private val endsWithBaar = """(.*)(baar)$""".r
  private val doesNotEndWithVowelOrI = """.*[^yaieouèI]$""".r
  private val duplicateVowel = """(.*[^yaieouè])(ee|aa|oo|uu)$""".r
  private val hedenSuffix = "heden$".r
  private val eneEnding = "ene?$".r
  private val seEnding = "se?$".r
  private val startWithY = "^y".r

  /** returns the stem of the given input*/
  def stem(input:String):Payload = {
        Pipeline(
                {(p: Payload) => Payload(p.word.toLowerCase, "lowercased" :: p.history)},
                {(p: Payload) => Payload(transpostAccents(p.word), "remapped accents" :: p.history)},
                {(p: Payload) => p.copy(word = iBetweenVowels.replaceAllIn(p.word, "$1I$2"))},
                {(p: Payload) => p.copy(word = startWithY.replaceFirstIn(p.word, "Y"))},
                {(p: Payload) => p.copy(word = yAfterVowels.replaceAllIn(p.word, "$1Y"))},
                step1(_:Payload),
                step2(_:Payload),
                step3a(_:Payload),
                step3b(_:Payload),
                step4(_:Payload),
                {(p: Payload) => Payload(p.word.toLowerCase, "lowercased" :: p.history)}
        ).exec(input)
  }
  
  /** removes one character from the end of the string if the end matches kk, dd or tt */ 
  def removeDuplicateEndings(input:Payload):String =
    if (duplicateEndingPattern.matches(input.word)) input.word >> 1 else input.word
  
  /**
   * Search for the longest among the following suffixes, and perform the action indicated
   *
   * heden:  replace with heid if in R1
   * en/ene: delete if in R1 and preceded by a valid en-ending, and then undouble the ending
   * s/se:   delete if in R1 and preceded by a valid s-ending 
   */
  def step1(input:Payload):Payload = {
    if(input.word.endsWith("heden")) { // separated into two if-statements to skip 'heden' as a word
      if(input.R1.endsWith("heden"))
        return Payload(hedenSuffix.replaceFirstIn(input.word, "heid"),  "replaced 'heden' by 'heid'" :: input.history)
    } else if(input.validEnEnding) {
      return Payload(removeDuplicateEndings(eneEnding.replaceFirstIn(input.word, "")), "'en(e)' removed" :: input.history)
    } else if(input.validSEnding) {
      return Payload(seEnding.replaceFirstIn(input.word, ""), "removed ending se?" :: input.history)
    }
    return input
  }

  /**
   * Step 2
   * 
   * Delete suffix e if in R1 and preceded by a non-vowel, and then undouble the ending
   */
  def step2(input:Payload):Payload = {
    if(input.R1.endsWith("e") && !isVowel(input.charBefore("e"))) 
      Payload(removeDuplicateEndings(input >> 1), "step 2 removed e" :: input.history) 
    else 
      input
  }

  /**
   * Step 3a
   *
   * delete heid if in R2 and not preceded by c, and treat a preceding en as in step 1(b)
   */
  def step3a(input:Payload):Payload = {
    if(input.R2.endsWith("heid") && input.charBefore("heid") != 'c'){ // delete 'heid' if in R2 and not preceded by c
      val res = input >> "heid"
      if(res.validEnEnding)
        Payload(removeDuplicateEndings(eneEnding.replaceFirstIn(res.word, "")), "step3a removed 'en(e)'" :: input.history )
      else
        Payload(res, input.history)
    }
    else
      input
  }

  /**  
   * Step 3b: d-suffixes (*)
   *
   * Search for the longest among the following suffixes, and perform the action indicated.
   *
   * end/ing: delete if in R2, if preceded by ig, delete if in R2 and not preceded by e, otherwise undouble the ending
   * ig:      delete if in R2 and not preceded by e
   * lijk:    delete if in R2, and then repeat step 2
   * baar:    delete if in R2
   * bar:     delete if in R2 and if step 2 actually removed an e (!) 
   */
  def step3b(input:Payload):Payload = {
	
    input.R2 match {
      case endsWithIngOrEnd(rest, suffix)  => processIngOrEnd(rest, suffix, input)
      case endsWithIgButNotEig()           => Payload(input >> "ig", ("3b removed 'ig'") :: input.history)
      case endsWithLijk(rest, suffix)      => {
        val afterStep2 = step2(Payload(input >> suffix, input.history))
        Payload(afterStep2.word, "removed 'lijk'" :: afterStep2.history)
      }
      case endsWithBar(rest, suffix)       => if(input.history.contains("step 2 removed e")) Payload(input >> suffix, "removed 'bar'" :: input.history) else input
      case endsWithBaar(rest, suffix)      => Payload(input >> suffix, "removed 'baar'" :: input.history)
      case _ => input
    }
  }

  private def processIngOrEnd(rest:String, suffix:String, input:Payload):Payload = {
    rest match {
      case endsWithIgButNotEig() => Payload(removeDuplicateEndings(input >> ("ig" + suffix)), ("removed " + "ig" + suffix) :: input.history)
      case _ => Payload(removeDuplicateEndings(input >> suffix), ("removed " + suffix) :: input.history)
    }
  }

  /**
   * Step 4: undouble vowel
   *
   * If the words ends CVD, where C is a non-vowel, D is a non-vowel other than I, and V is double a, e, o or u, remove one of the vowels from V (for example, maan -> man, brood -> brod). 
   */
  def step4(input:Payload):Payload = {
    input.word match {
      case doesNotEndWithVowelOrI() => Payload(checkForDuplicateVowelSuffix(input.word), "removed last character" :: input.history)
      case _ => input
    }
  }
 
  private def checkForDuplicateVowelSuffix(word:String):String = {
    word >> 1 match {
      case duplicateVowel(rest, suffix) => rest + suffix.last + word.last
      case _ => word
    }
  }
}