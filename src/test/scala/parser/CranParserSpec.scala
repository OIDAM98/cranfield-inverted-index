package parser
import org.scalatest.FunSpec
import scala.io.Source

class CranParserSpec extends FunSpec {
  describe("A Cran Formatter") {
    it("should parse all file") {
      val parser = new CranParser()
      val input = Source.fromResource("cran.all.1400")
      val parsed = parser.parseFile(input)
      assert(parsed.size == 1400)
    }
  }
}
