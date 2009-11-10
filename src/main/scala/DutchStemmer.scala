package com.log4p

import scala.util.matching.Regex.Match

case class Payload(val word:String, val history:List[String] = Nil) {
  val regionExpr = """.*?[yaieouè][^yaieouè](.+)""".r // string after first non-vowel following a vowel

  val R1 = findRegion(word) match {
    case Some(m:Match) => { // Adjust R1 according to German stemming rules: "so that the region before it contains at least 3 letters"
        if(m.start(1) < 3)
           if(word.length > 3) word.substring(3, word.length) else ""
        else
          m.group(1)
    }
    case None => ""
  }

  val R2 = findRegion(word) match {
    case Some(m:Match) => findRegion(m.group(1)) match {
        case Some(second:Match) => second.group(1)
        case None => ""
    }
    case None => ""
  }

  /** returns true if character before 'en' or 'ene' in the input is not a vowel, and not 'gem' */
  def validEnEnding:Boolean = R1.matches(""".*ene?$""") && !DutchStemmer.isVowel(charBeforeLast("en")) && !word.matches(".*gemene?$")

  /** returns true if character before 's' in the input is not a vowel and not 'j' */
  def validSEnding:Boolean = R1.matches(""".*se?$""") && !DutchStemmer.isVowel(charBeforeLast("s")) && charBeforeLast("s") != 'j'

  /** finds the region Some(Match) after the first non-vowel following a vowel, or None if there is no such non-vowel. */
  def findRegion(input:String) = regionExpr.findFirstMatchIn(input)
  /** returns the character before the given suffix. */
  def charBefore(suffix:String) = if(suffix.length < word.length)  word.charAt(word.size - suffix.size - 1) else ' '
  /** returns the last character before the given string */
  def charBeforeLast(str:String) = word.charAt(word.lastIndexOf(str)-1)
  /** like binary right-shift, drop num characters from the end of the string */
  def >> (num:Int) = word.substring(0, word.length - num)
  /** like binary right-shift, drop other.size characters from the end of the string */
  def >> (other:String) = word.substring(0, word.length - other.size)  
}

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

  def step2(input:Payload):Payload = {
    if(input.R1.endsWith("e") && !isVowel(input.charBefore("e"))) 
      Payload(removeDuplicateEndings(input >> 1), "step 2 removed e" :: input.history) 
    else 
      input
  }

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

  val endsWithIngOrEnd = """(.*)(ing|end)$""".r
  val endsWithLijk = """(.*)(lijk)$""".r
  val endsWithBar = """(.*)(bar)$""".r
  val endsWithBaar = """(.*)(baar)$""".r
  val endsWithIgButNotEig = """.*[^e]ig$|^ig$""".r

  def step3b(input:Payload):Payload = {
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
 
  val duplicateVowel = """(.*[^yaieouè])(ee|aa|oo|uu)$""".r
  val doesNotEndWithVowelOrI = """.*[^yaieouèI]$""".r

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
