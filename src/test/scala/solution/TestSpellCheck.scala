package solution

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.util.Random
import scala.collection.immutable.SortedSet
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class SpellTests extends FunSuite {
  
  //wordsEn.txt file needs to be in class path for the project
  val checker=SpellCheck(Source.fromURL(getClass.getResource("/wordsEn.txt")))
  
  test("word key corectly matches casing errors") {        
    assert(Word("inSIDE").key==Word("inside").key)
  }
  
  test("word key corectly repeat letters") {    
    assert(Word("jjoobbb").key==Word("job").key)
  }
  
  test("word key corectly matches incorrect vowels") {
    assert(Word("weke").key==Word("wake").key)
  }
  
  test("word key corectly matches combinations") {
    assert(Word("CUNsperrICY").key==Word("conspiracy").key)
  }
  
   
  test("fix invalid casing errors") {
    assert(checker.fix("inSIDE").get=="inside")
  }

  test("fix repeat letters") {
    assert(checker.fix("jjoobbb").get=="job")
  }

  test("fix incorrect vowels") {
    assert(checker.fix("weke").get=="wake")
  }

  test("fix combination - CUNsperrICY") {
    assert(checker.fix("CUNsperrICY").get=="conspiracy")
  }
  
  test("fix combination - peePPle") {
    assert(checker.fix("peePPle").get=="people")
  }
  
  test("fix combination - sheeeeep") {
    assert(checker.fix("sheeeeep").get=="sheep")
  }

 
  test("fix not found") {
    assert(checker.fix("sheeple").isEmpty)
  }
  
  test("check all words in a dictionary") {
    val lines=Source.fromURL(getClass.getResource("/wordsEn.txt")).getLines
    for(line<-lines){
      assert(checker.fix(line).get==line)
    }
  }
  
  test("check all misspellings in a dictionary") {
    val lines=Source.fromURL(getClass.getResource("/wordsEn.txt")).getLines
    for(line<-lines){
      val misspelledWord=Word(line).misspell.toString
      val suggestion=checker.fix(misspelledWord).get      
      //if(suggestion!=line) println(s"$line -> $misspelledWord ->$suggestion")
      assert(checker.fix(misspelledWord).isDefined)
    }
  }
  
  
 
}