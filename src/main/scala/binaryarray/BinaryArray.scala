package binaryarray

/*
This class represents a binary format for a word,
the indexes are the documents in which the word
appears in.
Has the capability to perform OR, AND and NOT
operations.
 */
final case class BinaryArray(private val arr: Seq[Short]) {
  // All the elements of this array must be either 0 or 1
  require(arr.forall(elem => elem == 0 || elem == 1))

  /*
  Returns the binary OR between the values of this
  array and the other array
   */
  def |(that: BinaryArray): BinaryArray = {
    val comparison = this.arr zip that.arr
    val result = comparison.map {
      case (f, s) => if (f == 1 || s == 1) 1.toShort else 0.toShort
    }
    BinaryArray(result)
  }

  /*
  Returns the binary AND between the values of this
  array and the other array
   */
  def &(that: BinaryArray) = {
    val comparison = this.arr zip that.arr
    val result = comparison map {
      case (f, s) => if (f == 1 && s == 1) 1.toShort else 0.toShort
    }
    BinaryArray(result)
  }

  /*
  Returns a BinaryArray that contains the binary not
  of the values inside this array
   */
  def not: BinaryArray = {
    val result = this.arr map (i => if (i == 1) 0.toShort else 1.toShort)
    BinaryArray(result)
  }

  /*
  Sintactic sugar for the not method.
  Allows to write it like: ~array
   */
  def unary_~ : BinaryArray = this.not
  def apply(index: Int): Short = arr(index)

  // Returns a new array with the index updated
  def update(index: Int, elem: Short): BinaryArray =
    BinaryArray(this.arr.updated(index, elem))

  // Returns the values of this array as the primitive type
  def toArray = this.arr
}

final case object BinaryArray {
  def apply(x: Short, xs: Short*): BinaryArray = BinaryArray(x +: xs)
  def fill(n: Int)(x: => Short) = BinaryArray(Array.fill(n)(x))
}
