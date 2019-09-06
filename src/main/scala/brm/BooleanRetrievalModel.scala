package brm

import index.InvertedIndex
import binaryarray.BinaryArray
import parser.Id
import parser.QueryParser
import scala.util.parsing.combinator.RegexParsers

class BooleanRetrievalModel(index: InvertedIndex) {

  private val parser: QueryParser = new QueryParser(this)

  def convertToBinaryArray(word: String) = {
    index.get(word) match {
      case Some(value) =>
        value match {
          case (_, documents) =>
            documents
              .map(_.id)
              .foldLeft(BinaryArray.fill(index.numberOfDocs)(0)) {
                case (acc, index) => acc.update(index - 1, 1)
              }
        }

      case None => BinaryArray.fill(index.numberOfDocs)(0)
    }
  }

  def resolveQuery(query: String): List[Int] = {
    parser.parseAll(parser.expr, query.toLowerCase) match {
      case parser.Success(result, _) =>
        result.toArray.zipWithIndex.foldRight(List.empty[Int]) {
          case ((value, i), acc) => if (value == 1) i + 1 :: acc else acc
        }
      case failure: parser.NoSuccess => List(-1)
    }
  }

}
