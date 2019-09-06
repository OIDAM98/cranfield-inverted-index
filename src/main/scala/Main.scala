import index.InvertedIndex
import brm.BooleanRetrievalModel
import scala.io.StdIn.readLine

object Main extends App {
  val queryOp = """The query has to be of this type:
    |"word (AND|OR [NOT] word)*"
    |
    |An AND|OR operator has to divide the words.""".stripMargin
  val options = """Enter :exit to quit the application
    |Enter :query to see the format of a query""".stripMargin
  val start = "Enter a query to be search..."

  println("Starting...")
  // Parse the document and create an Inverted Index based upon it
  val index = new InvertedIndex("cran.all.1400")
  // Start the Boolean Retrieval Model with the index last created
  val brm = new BooleanRetrievalModel(index)

  // Print the menu to the user
  println(start + "\n" + queryOp + "\n" + options)

  var query: String = ""
  var check: Boolean = false

  // While the user hasn't typed :exit
  while (!check) {
    // Read the line (query) the user enters
    query = readLine("query> ")
    // Check if it's the exit command
    if (query.trim == ":exit")
      check = true
    // Check if the user wants the query options
    else if (query.trim == ":query")
      println(queryOp)
    // The user entered a query to be searched for
    else {

      val results = brm.resolveQuery(query)
      results.headOption match {
        case Some(value) if value < 0 =>
          println("There was an error with the query, try again...")
        case Some(value) => {
          val formatted = "The results are:\n" + results
            .map(i => s"Document $i")
            .sliding(5, 5)
            .map(_.mkString(", "))
            .mkString("\n") +
            "\n"
          println(formatted)
        }
        case None => println("No documents where found for the query")
      }
    }
  }

}
