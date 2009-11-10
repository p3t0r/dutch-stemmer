package com.log4p

import scala.util.matching.Regex.Match

/**
 * Implementation of a stemming algorithm for the Dutch language.
 *
 * The full specification on which this code is based can be found at:
 *        <a href="http://snowball.tartarus.org/algorithms/dutch/stemmer.html">http://snowball.tartarus.org/algorithms/dutch/stemmer.html</a>
 *
 * @author Peter Maas (pfmmaas [at] gmail [dot] com)
 */
object DutchStemmer {
  implicit def str2payload(s:String) = Payload(s)

  /** returns the stem of the given input*/
  def stem(input:String):Payload = {
    pipeline.foldLeft(Payload(input)) { (previousOutput, step) =>
      step.apply(previousOutput)
    }
  }

  val iBetweenVowels = "([yaieouè]+)i([yaieouè]+)"
  val yAfterVowels = "([yaieouè]+)y"

  private val pipeline = List(
      {p:Payload => Payload(p.word.toLowerCase, "lowercased" :: p.history)},
      {p:Payload => Payload(remapAccentedVowels(p), "remapped accents" :: p.history)},  // First, remove all umlaut and acute accents. A vowel is then one of 'aeiouyè'
      {p:Payload => Payload(p.word.replaceAll(iBetweenVowels,"$1I$2"), p.history)},     // Put i between vowels into upper case
      {p:Payload => Payload(p.word.replaceAll("^y","Y"), p.history)},                   // Put y at the beginning into upper case
      {p:Payload => Payload(p.word.replaceAll(yAfterVowels,"$1Y"), p.history)},         // Put y after a vowel into upper case
      step1(_:Payload),
      step2(_:Payload),
      step3a(_:Payload),
      step3b(_:Payload),
      step4(_:Payload),
      {p:Payload => Payload(p.word.toLowerCase, "lowercased" :: p.history)}
    )

  /** remaps all accented vowel in the input string to the specified counterparts */
  def remapAccentedVowels(input:Payload):String = input.word.toCharArray.map(transposeAccent(_)).mkString
  /** removes one character from the end of the string if the end matches kk, dd or tt */ 
  def removeDuplicateEndings(input:Payload):String = if(input.word.matches(".*(kk|dd|tt)$")) input.word >> 1 else input.word
  
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
	  	return Payload(input.word.replaceAll("heden$","heid"),  "replaced 'heden' by 'heid'" :: input.history)
    } else if(input.validEnEnding) {
      return Payload(removeDuplicateEndings(input.word.replaceAll("ene?$","")), "'en(e)' removed" :: input.history)
    } else if(input.validSEnding) {
      return Payload(input.word.replaceAll("se?$",""), "removed ending se?" :: input.history)
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
        Payload(removeDuplicateEndings(res.replaceAll("ene?$","")), "step3a removed 'en(e)'" :: input.history ) 
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
  private val endsWithIgButNotEig = """.*[^e]ig$|^ig$""".r
  def step3b(input:Payload):Payload = {
	val endsWithIngOrEnd = """(.*)(ing|end)$""".r
	val endsWithLijk = """(.*)(lijk)$""".r
	val endsWithBar = """(.*)(bar)$""".r
	val endsWithBaar = """(.*)(baar)$""".r
	
	val initR2 = input.R2
	input.R2 match {
      case endsWithIngOrEnd(rest, suffix)  => processIngOrEnd(rest, suffix, input)
      case endsWithIgButNotEig()           => Payload(input >> "ig", ("3b removed 'ig'") :: input.history)
      case endsWithLijk(rest, suffix)      => Payload(step2(Payload(input >> suffix, input.history)).word, "removed 'lijk" :: input.history)
      case endsWithBar(rest, suffix)       => if(input.history.contains("step 2 removed e")) Payload(input >> suffix, "removed 'bar'" :: input.history) else input
      case endsWithBaar(rest, suffix)      => Payload(input >> suffix, "removed 'baar" :: input.history)
      case _ => input
    }
  }

  private def processIngOrEnd(rest:String, suffix:String, input:Payload):Payload = {
    rest match {
      case endsWithIgButNotEig() => Payload(removeDuplicateEndings(input >> ("ig" + suffix)), ("removed " + "ig" + suffix) :: input.history)
      case _ => Payload(removeDuplicateEndings(input >> suffix), ("removed " + suffix) :: input.history)
    }
  }
 
  def step4(input:Payload):Payload = {
	val doesNotEndWithVowelOrI = """.*[^yaieouèI]$""".r
	input.word match {
		case doesNotEndWithVowelOrI() => Payload(checkForDuplicateVowelSuffix(input.word), "removed last character" :: input.history)
		case _ => input
	}
  }
 
  private def checkForDuplicateVowelSuffix(word:String):String = {
	val duplicateVowel = """(.*[^yaieouè])(ee|aa|oo|uu)$""".r
	word >> 1 match {
	  case duplicateVowel(rest, suffix) => rest + suffix.last + word.last
	  case _ => word
	}
  }
 
  private def transposeAccent(c:Char):Char = {
    c match {
      case 'ä' => 'a'
      case 'ë' => 'e'
      case 'ï' => 'i'
      case 'ö' => 'o'
      case 'ü' => 'u'
      case 'á' => 'a'
      case 'é' => 'e'
      case 'í' => 'i'
      case 'ó' => 'o'
      case 'ú' => 'u'
      case  _  => c // other accents, like è and à should remain untouched
    }
  }

  def isVowel(c:Char):Boolean = {
    c match {
      case 'a' => true
      case 'e' => true
      case 'i' => true
      case 'o' => true
      case 'u' => true
      case 'y' => true
      case 'è' => true
      case  _  => false
    }
  }
}
