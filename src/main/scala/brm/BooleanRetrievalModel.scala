package brm

import index.InvertedIndex
import binaryarray.BinaryArray
import parser.Id
import parser.QueryParser
import scala.util.parsing.combinator.RegexParsers

class BooleanRetrievalModel(index: InvertedIndex) {

  private val parser: QueryParser = new QueryParser(this)

  /*
  Converts a word into its binary representation.
  This means that it searches through the Index if the word
  exists and gets the Ids where the word is
   */
  def convertToBinaryArray(word: String) = {
    index.get(word) match {
      case Some(value) =>
        value match {
          case (_, documents) =>
            documents
              .map(_.id)
              .foldLeft(BinaryArray.fill(index.numberOfDocs)(0)) {
                // Fills with 1 the index of the document where the word appears in
                // Since the array is 0-based and the Ids are 1-based index then
                // substracts 1 from the id to match the array's index
                case (acc, index) => acc.update(index - 1, 1)
              }
        }

      case None => BinaryArray.fill(index.numberOfDocs)(0)
    }
  }

  /*
  Parses the query and gets its results.
  If the parser couldn't parse the query then it returns a List
  with its head as -1
   */
  def resolveQuery(query: String): List[Int] = {
    parser.parseAll(parser.expr, query.toLowerCase) match {
      // Collects the results of the BinaryArray into a List
      // of integers that represent the desired documents
      // that match the query
      case parser.Success(result, _) =>
        result.toArray.zipWithIndex.foldRight(List.empty[Int]) {
          case ((value, i), acc) => if (value == 1) i + 1 :: acc else acc
        }
      case failure: parser.NoSuccess => List(-1)
    }
  }

}
