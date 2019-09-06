package index

package index

import org.scalatest._
import parser.CranParser

class InvertedIndexSpec extends FunSpec {
  describe("The Inverted Index") {
    it("should create an index for the whole document") {
      val index = new InvertedIndex("cran.all.1400")
      assert(!index.index.isEmpty)
    }
  }
}