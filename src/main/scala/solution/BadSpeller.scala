package solution

import scala.io.Source

object BadSpeller extends App{
      
  for( ln <- Source.fromURL(getClass.getResource("/wordsEn.txt")).getLines){
    System.out.println(Word(ln).misspell)
  }
    
}