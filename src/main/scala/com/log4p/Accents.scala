package com.log4p

object Accents {

  /** remaps all accented vowel in the input string to the specified counterparts */
  def transposeAccents(s:String):String = s.toCharArray.map(transposeAccent(_)).mkString
	
  def transposeAccent(c:Char):Char = {
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