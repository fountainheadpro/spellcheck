package solution

import scala.io.Source
import collection.mutable.StringBuilder
import scala.util.matching.Regex
import scala.annotation.tailrec

case class Word(word: String){
      
  private val vowelsStr="aeiouAEIOU"
  private val vowels=vowelsStr.toSet
  
  //building an invariant against spell check rules  
  def key=fixCase.fixVowels.fixDups
    
  def fixCase=Word(word.toLowerCase())
    
  def fixDups=Word(replaceDups(word, None))
           
  //replacing vowels with * as invariant
  def fixVowels: Word=Word(word.map((x)=>if (vowels.contains(x)) '*' else x))
  
  def isWord: Boolean={
    val validChars=Set(''')
    def validChar(c: Char): Boolean=c.isLetter | validChars.contains(c)   
    word.foldLeft(true)(_ & validChar(_))
  }
  
  private def replaceDups(s: String, r: Option[Char]): String={
    val b = StringBuilder.newBuilder    
    var prev: Char=' '
    val hasReplacement=(!r.isEmpty)
    var replaced=false
    for (x <- s) {
      if (x!=prev){
        b += x
        replaced=false
      }
      else{
        if (hasReplacement && !replaced){
          b += r.get
          replaced=true
        }
      }
      prev=x
    }
    b.result     
  }
  
  
  def misspell(): Word={
    
    val replaceRandomVowel  = (c: Char) => (if (vowels.contains(c)) util.Random.shuffle(vowels.toList).head else c).toString
    val introduceRendomCase = (c: Char) => if (util.Random.nextBoolean) c.toString().toUpperCase else c.toString().toLowerCase
    val introduceDuplicates = (c: Char) => c.toString*(util.Random.nextInt(4)+1)
    
    
    val errors=List(replaceRandomVowel,introduceRendomCase,introduceRendomCase)
    
    val b = StringBuilder.newBuilder    
    for (x <- word) {
      b ++= util.Random.shuffle(errors).head(x)
    }
    Word(b.result)         
  }
  
  def toRegEx: Regex={
    val dupsReplaced=replaceDups(word.toLowerCase, Option('+'))
    val letterBasedRegex=dupsReplaced.map((x)=>x match {
      case '+' => "+"
      case vowel if(vowels.contains(vowel))=>s"[$vowelsStr]"  
      case x=>"["+x.toString().toUpperCase+x.toString().toLowerCase+"]"
    })
    letterBasedRegex.mkString("").r        
  }  
  

  def toRegMatchVowels: Regex={
    val dupsReplaced=replaceDups(word.toLowerCase, Option('+'))
    val letterBasedRegex=dupsReplaced.map((x)=>x match {
      case '+' => "+"
      case x=>"["+x.toString().toUpperCase+x.toString().toLowerCase+"]{1}"
    })
    letterBasedRegex.mkString("").r        
  }
    
  override
  def toString=word
  
}

class SpellCheck(dictionary: Map[Word, Set[String]]){
   
  def fix(input: String): Option[String]={
        
    //going through matches to narrow the search via list of filters 
    @tailrec 
    def narrowSuggestions(matches: Set[String], filters: List[Function1[Set[String], Set[String]]]):Option[String]=matches match {    
	    case empty            if (empty.size==0)           => None
	    case deterministic    if (deterministic.size==1)   => deterministic.headOption
	    case nonDeterministic if (nonDeterministic.size>1) =>
	      filters.headOption match {
  	        case None         =>  nonDeterministic.headOption //no more filters left - taking the first suggestion
	        case Some(filter) =>  narrowSuggestions(filter(nonDeterministic), filters.tail)
	      }	          
	  }

    
    //trying to enhance matches to match exact vowels
    //if no match found, returning original set for the next filter  
    val lengthAndMostVovelsMatchFilter=(nonDeterministic: Set[String])=>{
      
      def matchingVowelsCount(suggestion: String): Int=(for(c1<-suggestion; c2<-input if c1.toLower==c2.toLower) yield c1).length
      
      val sameLengthSuggestions=for(suggestion<-nonDeterministic if (suggestion.length()==input.length)) yield suggestion
      if (!sameLengthSuggestions.isEmpty){
    	  sameLengthSuggestions.groupBy(matchingVowelsCount).maxBy(_._1)._2
      }
      else{
        nonDeterministic
      }      
    }
    
    //trying to fix duplicates by matching exact vowels  
    val excatVowelsDuplicatesIgnoredFilter=(nonDeterministic: Set[String])=>{
      val regex=Word(input).toRegMatchVowels
      nonDeterministic.filter(regex.findFirstIn(_).isDefined) match {
        case empty    if (empty.size==0)   => nonDeterministic
        case nonEmpty if (nonEmpty.size>0) => nonEmpty
      }     
    }

    //trying to enhance matches to match exact vowels words ignoring case
    val caseInsencetiveExcatMatchFilter=(nonDeterministic: Set[String])=>{
      nonDeterministic.filter(_.toLowerCase()==input.toLowerCase()) match {        
        case empty if (empty.size==0)=> nonDeterministic
        case nonEmpty if (nonEmpty.size>0)=>nonEmpty
      }
    }    
    
    val inputWord=Word(input)
    if (inputWord.isWord){    
	    //lookup the word in the dictionary based on key invariant. It can return one or more suggestions
	    val suggestions=dictionary.getOrElse(Word(input).key, Set())
	    val filters=List(caseInsencetiveExcatMatchFilter, lengthAndMostVovelsMatchFilter, excatVowelsDuplicatesIgnoredFilter)
	    //narrow suggestions to the best suggestion base on the filters
	    val result=narrowSuggestions(suggestions, filters)    
	    result
    }
    else
    {
      sys error "Only words are accepted."
    }
  }
  
  def dictionaryStats=dictionary.map {_._2.size}.groupBy((x)=>x).mapValues(_.size.toFloat/dictionary.size*100)
      
} 
  
object SpellCheck{
  
  //building dictionary with lookup key as invariant against the spell check rules
  def apply(source: Source): SpellCheck={
    val lines=source.getLines
    val populatedDictionary: Map[Word, Set[String]]=lines.foldLeft(Map[Word, Set[String]]()){
      (dictionary, line: String) =>
        val newWord=Word(line)
        if (newWord.isWord){
        	val key=newWord.key
        	dictionary+(key -> (dictionary.getOrElse(key, Set())+line))
        }
        else
        {
          sys error s"Only words are accepted to the dictionary. Failed on this line: $line"
        }
    }
    new SpellCheck(populatedDictionary)
  }
  
    
}
  
object Cli extends App{
  
  def dictionaryFile=args match{
      case Array(fileName)=>Source.fromFile(fileName)
      case _=> Source.fromURL(getClass.getResource("/wordsEn.txt"))
    }
    
  
  try{  
	val checker=SpellCheck(dictionaryFile)
	System.out.print(">")
	for( ln <- io.Source.stdin.getLines){
	  try{
	   checker.fix(ln) match {
	     case None=>println("NO SUGGESTION")
		 case Some(s)=>println(s)
		}
	  } 
	  catch{
	    case e: RuntimeException => println(e.getMessage())
	  }    
      System.out.print(">")
	}
  }catch{
    case ex:java.io.FileNotFoundException => System.out.print("Error: Dictionary File Not Found.")
    case ex:NullPointerException          => System.out.print("Internal dictionary not found.")
  }
  
}