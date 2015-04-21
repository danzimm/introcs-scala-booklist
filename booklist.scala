// vim: ts=3:sts=3:sw=3

import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.io.Source

// We provide the Book class, which is used to hold book information.
// You will use it to make a book list.

case class Book(title : String, author : String, year : Int)

// Your job is to replace all ??? with your own code.

class BookList {
   var list = ListBuffer[Book]()

   def addBook(book : Book) : Unit = {
      list += book
   }

   def getNumberOfBooks() : Int = list.length
      // instructor has done this for you
      // using it for testing purposes

   def printList() : Unit = {
      val str: String = list map { (b: Book) =>
         b.title + " by " + b.author + " in " + b.year
      } reduceLeft { (acc: String, x: String) =>
         acc + "\n" + x
      }
      println(str)
   }

   def getTitlesByAuthor(author : String) : ListBuffer[String] = {
      list filter { (b: Book) => b.author == author } map { b => b.author }
   }

   def getTitlesContaining(substring : String) : ListBuffer[String] = {
      list filter { (b: Book) => b.title.contains(substring) } map { b => b.title }
   }

   def getBooksBetweenYears(firstYear : Int, lastYear : Int) : ListBuffer[Book] = {
      list filter { b => firstYear <= b.year && b.year <= lastYear } 
   }

   def addFromFile(name : String) : Unit = {
     // instructor did this one for you...mostly
     for (file <- Try(Source.fromFile(name))) {
        for (line <- file.getLines) {
           val parts = line.split(":")
           if (parts.length >= 3) {
              val title = parts(0)
              val author = parts(1)
              val year = Try(parts(2).toInt).getOrElse(0)
              list += Book(title, author, year)
           } else {
              println("Ignoring: " + line)
           }
        } 
     } 
   }

   def addAll(books : BookList) : Unit = {
      // instructor did this one for you, too
      books.list foreach { n => list += n }
   }
}

