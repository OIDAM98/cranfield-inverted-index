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
  /*
  Takes the filename, opens it, parses the document and creates an inverted
  index from it
   */
  def this(documentName: String) {
    this()
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
        val regex = """(\s|\.|,|-|\\|/|\(|\))+"""
        p.map {
          case Document(tags) =>
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

    def zipWithDocument: List[(Array[String], Id)] => List[(String, Set[Id])] =
      words =>
        words flatMap {
          case ((arr, id)) => {
            arr.zip(Stream.continually(Set(id)))
          }
        }

    def groupByIdentity
        : List[(String, Set[Id])] => Map[Term, List[(String, Set[Id])]] =
      tuples =>
        tuples.groupBy {
          case (word, _) => identity(word)
        }

    def reduceIndex: Map[Term, List[(String, Set[Id])]] => Index =
      index =>
        index
          .mapValues(
            _.foldLeft((0, Set.empty): PostingList) {
              case ((count, set), (word, doc)) => (count + 1, set union doc)
            }
          )
          .toMap

    val generateIndex = splitWords andThen zipWithDocument andThen groupByIdentity andThen reduceIndex

    generateIndex(parsed)

  }

  def get(word: String): Option[(Int, Set[Id])] = this.index.get(word)

}
