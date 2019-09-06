package brm

import org.scalatest._
import index.InvertedIndex

class BooleanRetrievalModelSpec extends FunSpec {
  describe("The Boolean Retrieval Model") {
    it("should answer a one word query") {
      val index = new InvertedIndex("cran.all.1400")
      val brm = new BooleanRetrievalModel(index)

      val results = brm.resolveQuery("gradient");
      assert(!results.isEmpty)
    }

    it("should answer a NOT query") {
      val index = new InvertedIndex("cran.all.1400")
      val brm = new BooleanRetrievalModel(index)

      val results = brm.resolveQuery("NOT gradient");
      assert(!results.isEmpty)
    }

    it("should answer an AND query") {
      val index = new InvertedIndex()
      val brm = new BooleanRetrievalModel(index)

      val results1 = brm.resolveQuery("gradient AND the");
      val results2 = brm.resolveQuery("gradient AND index")

      assert(!results1.isEmpty)
      assert(results2.isEmpty)
    }

    it("should answer an OR query") {
      val index = new InvertedIndex("cran.all.1400")
      val brm = new BooleanRetrievalModel(index)

      val results1 = brm.resolveQuery("gradient OR the");
      val results2 = brm.resolveQuery("gradient OR destalling")

      assert(!results1.isEmpty)
      assert(!results2.isEmpty)
    }

    it("should answer a complex query") {

      val index = new InvertedIndex("cran.all.1400")
      val brm = new BooleanRetrievalModel(index)

      val results = brm.resolveQuery("gradient or index and not (destalling or index)");
      assert(!results.isEmpty)
    }

  }
}
