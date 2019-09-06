package index

import parser._
import scala.io.Source
import scala.collection.immutable.BitSet

class InvertedIndex {

  final type Term = String
  final type PostingList = (Int, Set[Id])
  final type Index = Map[Term, PostingList]

  // Component to parse and tokenize the Cran document
  private val parser: CranParser = new CranParser()
  // Name of the document to Parse (Cran)
  private var documentName: String = ""
  var numberOfDocs: Int = 0
  // Stores the inverted index inside a variable to keep it in memory
  var index: Index = null

  // Auxiliary constructor, recieves a document name to parse
  def this(documentName: String) {
    this()
    /*
    Takes the filename, opens it, parses the document and creates an inverted
    index from it
     */
    this.documentName = documentName
    val input = Source.fromResource(documentName)
    val parsed = parser.parseFile(input)
    setNumberOfDocs(parsed.length)
    this.index = createIndex(parsed)
  }

  def setNumberOfDocs(num: Int) = this.numberOfDocs = num

  def createIndex(parsed: List[Document]) = {

    /*
     */
    def splitWords: List[Document] => List[(Array[String], Id)] =
      p => {
        // Splits the word of the documents by these special characters:
        // Whitespace, -, /, ), (, ., ,
        val regex = """(\s|\.|,|-|\\|/|\(|\))+"""
        p.map {
          case Document(tags) =>
            // For each header tag in the document, create a Tuple of (Array[String], Id)
            // that represents the words of the document and the id of the document
            tags.foldLeft(
              (Array.empty[String], Id(0)): Tuple2[Array[String], Id]
            ) {
              case ((arr, id), curr) =>
                curr match {
                  case Abstract(text) =>
                    (arr ++ text.split(regex).map(_.trim), id)
                  case Adscription(adscription) =>
                    (arr ++ adscription.split(regex).map(_.trim), id)
                  case Author(fullName) =>
                    (arr ++ fullName.split(regex).map(_.trim), id)
                  case Title(text) => (arr ++ text.split(regex).map(_.trim), id)
                  case newId: Id   => (arr, newId)
                }
            }
        }
      }

    // Zips each word with its corresponding id to flatten the Array[String] recieved
    def zipWithDocument: List[(Array[String], Id)] => List[(String, Set[Id])] =
      words =>
        words flatMap {
          case ((arr, id)) => {
            arr.zip(Stream.continually(Set(id)))
          }
        }

    /*
        Groups the words based upon identity and creates a HashMap
        where the keys are the words and the values are
        both the number of times it is repeated in the whole document
        and a set of Ids where it is found
     */
    def groupByIdentity
        : List[(String, Set[Id])] => Map[Term, List[(String, Set[Id])]] =
      tuples =>
        tuples.groupBy {
          case (word, _) => identity(word)
        }

    /*
        Creates the final version of the idnex where it counts the number of times a word
        is repeated in the documents and unites the sets where the word
        is encountered
     */
    def reduceIndex: Map[Term, List[(String, Set[Id])]] => Index =
      index =>
        index
          .mapValues(
            _.foldLeft((0, Set.empty): PostingList) {
              case ((count, set), (word, doc)) => (count + 1, set union doc)
            }
          )
          .toMap

    // The creation of the InvertedIndex is the composition
    // of all the functions defined above
    val generateIndex = splitWords andThen zipWithDocument andThen groupByIdentity andThen reduceIndex

    generateIndex(parsed)

  }

  // Searched for the word in the index and returns the results as an Option
  def get(word: String): Option[(Int, Set[Id])] = this.index.get(word)

}
