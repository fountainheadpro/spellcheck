package solution

import scala.io.Source
import collection.mutable.StringBuilder
import scala.collection.SortedSet
import scala.util.matching.Regex

case class Word(word: String){
  
  private val vowels="aeiouAEIOU".toSet
  
  //building an invariant against spellcheck rules  
  def key=fixCase.fixVowels.fixDups
    
  def fixCase=Word(word.toLowerCase())
    
  def fixDups=Word(replaceDups(word, None))
           
  def fixVowels: Word=Word(word.map((x)=>if (vowels.contains(x)) '*' else x))
  
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
        if (hasReplacement && !replaced) {
          b += r.get
          replaced=true
        }
      }
      prev=x
    }
    b.result     
  }
  

  def toRegMatchVowels: Regex={
    val dupsReplaced=replaceDups(word.toLowerCase, Option('+'))
    val letterBasedRegex=dupsReplaced.map((x)=>x match {
      case '+' => "+"
      case x=>"["+x.toString().toUpperCase+x.toString().toLowerCase+"]"
    })
    letterBasedRegex.mkString("").r        
  }
  
  def toRegMatchVowelsAndDuplicates: Regex={    
    val letterBasedRegex=word.map((x)=>x match {
      case '+' => "+"
      case x=>"["+x.toString().toUpperCase+x.toString().toLowerCase+"]"
    })
    letterBasedRegex.mkString("").r        
  }
  

}

class SpellCheck(dictionary: Map[Word, Set[String]]){
   
  def fix(input: String): Option[String]={
        
    //going through matches to narrow the search via list of filters 
    def narrowSuggestions(matches: Set[String], filters: List[Function1[Set[String], Set[String]]]):Option[String]=matches match {    
	    case empty if (empty.size==0)=>None
	    case deterministic if (deterministic.size==1)=>deterministic.headOption
	    case nonDeterministic if (nonDeterministic.size>1)=>filters.headOption match {
	      case None=>nonDeterministic.headOption
	      case Some(filter)=>{	            
	        narrowSuggestions(filter(nonDeterministic), filters.tail)
	      }
	    }	          
	  }

    //trying to enhance matches to match exact vowels
    //if no match found, returning original set for the next filter  
    val caseInsencetiveDuplicatesIgnoredFilter=(nonDeterministic: Set[String])=>{    
      val regex=Word(input).toRegMatchVowels
      nonDeterministic.filterNot(regex.findFirstIn(_).isEmpty) match {
        case empty if (empty.size==0)=>nonDeterministic
        case nonEmpty if (nonEmpty.size>0)=>nonEmpty
      }     
    }

    //trying to enhance matches to match exact vowels words ignoring case
    val caseInsencetiveExcatMatchFilter=(nonDeterministic: Set[String])=>{
      nonDeterministic.filter(_.toLowerCase()==input.toLowerCase()) match {        
        case empty if (empty.size==0)=> nonDeterministic
        case nonEmpty if (nonEmpty.size>0)=>nonEmpty
      }
    }    
    
    val suggestions=dictionary.getOrElse(Word(input).key, Set())
    val result=narrowSuggestions(suggestions, List(caseInsencetiveExcatMatchFilter, caseInsencetiveDuplicatesIgnoredFilter))    
    result
  }
      
} 
  
object SpellCheck{
  
  //building dictionary with lookup key as invariant against the spell check rules
  def apply(file: String): SpellCheck={
    val lines=Source.fromFile(file).getLines
    val populatedDictionary: Map[Word, Set[String]]=lines.foldLeft(Map[Word, Set[String]]()){
      (dictionary, word: String) =>
        val newWord=Word(word)
        val key=newWord.key
        dictionary+(key -> (dictionary.getOrElse(key, Set())+word))
    }
    new SpellCheck(populatedDictionary)
  }
  
    
}
  
object Cli extends App{
  
  val checker=SpellCheck(args(0))
  System.out.print(">")
    
  for( ln <- io.Source.stdin.getLines ){
	  checker.fix(ln) match {
	    case None=>println("NO SUGGESTION")
	    case Some(s)=>println(s)
	  }
	  System.out.print(">")
  }
  
  
}






