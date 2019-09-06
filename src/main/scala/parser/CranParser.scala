package parser
import java.io.BufferedReader
import scala.io.Source

// Classes that represent the data found within the document
sealed trait Header
case class Id(id: Int) extends Header
case class Title(text: String) extends Header
case class Author(fullName: String) extends Header
case class Adscription(adscription: String) extends Header
case class Abstract(text: String) extends Header
case class Document(tags: List[Header])

/**
  * Class to parse the Cran.All.1400 document
  * Iterates over the document, splitting by Id (.I)
  * then divides the result by the headers (.[A-Z])
  * and creates the corresponding class to represent the
  * data found.
  */
class CranParser {

  /*
  Splits a string by ".I" in order to arrange the string
  into the documents and its tags. Also removes the
  empty strings that the split function leaves.
   */
  private def splitById: String => Array[String] =
    buff => buff.split(".I").filterNot(_.isEmpty)

  private def splitByHeaders: Array[String] => List[Document] = docs => {
    /*
    Pattern to split the document by ".[A-Z]", uses Lookbehind and Lookahead
    patterns to keep the delimeter after making the split.
    This allows to classify the text after making the split.
     */
    val pattern: String = """((?<=(\.[A-Z]))|(?=(\.[A-Z])))"""
    //Split the string by headers
    val splitted: Array[Array[String]] = docs.map(_.split(pattern))
    splitted
      .map(arr => {
        val first: Id = Id(arr.head.trim.toInt) // The first element is always the ID of the document
        val everything = arr.tail.zipWithIndex // Save each string with its index in the array
        /*
        In the array the tags are on the even indexes and the text to parse
        is on the uneven indexes. So we need to retrieve both of them and
        zip them together to better process them into their specific classes.
        [.T, "...", .A, "...", ...] => [(.T, "..."), (.A, "..."), ...]
         */
        val tags = everything.filter(_._2 % 2 == 0).map(_._1)
        val text = everything.filterNot(_._2 % 2 == 0).map(_._1)
        /*
        Fold through the zipped headers and text to convert them into a
        specific class. Since the first element of the tuple is the header
        (.[A-Z], text) this element is used to create the specific class
        of the text. This process is stored inside a List to improve
        various headers while keeping the order.
        Nevertheless, the order is not essential to this process so there's
        no need to reverse the FoldRight.
         */
        val parsed = first :: ((tags zip text)
          .foldRight(List.empty[Header]) {
            case ((header, text), acc) =>
              header match {
                case ".T" => Title(text.trim) :: acc
                case ".A" => Author(text.trim) :: acc
                case ".B" => Adscription(text.trim) :: acc
                case ".W" => Abstract(text.trim) :: acc
              }
          })
        //Return the headers parsed inside a Document class
        Document(parsed)
      })
      .toList

  }

  // This method is the composition of the past functions in order to
  // convert a String into a List of Document's
  private def convertToDocuments: String => List[Document] =
    splitById andThen splitByHeaders

  // Takes a Source and converts it to a List of Documents
  def parseFile(file: Source): List[Document] =
    convertToDocuments(file.mkString)

}
