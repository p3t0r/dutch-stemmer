package com.log4p

import scala.util.matching.Regex.Match


/**
 * Payload object is passed through the stemming pipeline. It contains various helper methods which a used by the algorithm.
 *
 * @author Peter Maas (pfmmaas [at] gmail [dot] com)
 */
case class Payload(val word:String, val history:List[String] = Nil) {
  import Accents._
	
  val regionExpr = """.*?[yaieouè][^yaieouè](.+)""".r // string after first non-vowel following a vowel
  /** finds the region Some(Match) after the first non-vowel following a vowel, or None if there is no such non-vowel. */
  def findRegion(input:String) = regionExpr.findFirstMatchIn(input)

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
  def validEnEnding:Boolean = R1.matches(""".*ene?$""") && !isVowel(charBeforeLast("en")) && !word.matches(".*gemene?$")

  /** returns true if character before 's' in the input is not a vowel and not 'j' */
  def validSEnding:Boolean = R1.matches(""".*se?$""") && !isVowel(charBeforeLast("s")) && charBeforeLast("s") != 'j'

  /** returns the character before the given suffix. */
  def charBefore(suffix:String) = if(suffix.length < word.length)  word.charAt(word.size - suffix.size - 1) else ' '

  /** returns the last character before the given string */
  def charBeforeLast(str:String) = word.charAt(word.lastIndexOf(str)-1)

  /** like binary right-shift, drop num characters from the end of the string */
  def >> (num:Int) = word.substring(0, word.length - num)
 
 /** like binary right-shift, drop other.size characters from the end of the string */
  def >> (other:String) = word.substring(0, word.length - other.size)  
}