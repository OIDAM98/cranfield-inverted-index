package binaryarray

final case class BinaryArray(private val arr: Seq[Short]) {
  require(arr.forall(elem => elem == 0 || elem == 1))

  def |(that: BinaryArray): BinaryArray = {
    val comparison = this.arr zip that.arr
    val result = comparison.map {
      case (f, s) => if (f == 1 || s == 1) 1.toShort else 0.toShort
    }
    BinaryArray(result)
  }

  def &(that: BinaryArray) = {
    val comparison = this.arr zip that.arr
    val result = comparison map {
      case (f, s) => if (f == 1 && s == 1) 1.toShort else 0.toShort
    }
    BinaryArray(result)
  }

  def not: BinaryArray = {
    val result = this.arr map (i => if (i == 1) 0.toShort else 1.toShort)
    BinaryArray(result)
  }

  def unary_~ : BinaryArray = this.not
  def apply(index: Int): Short = arr(index)
  def update(index: Int, elem: Short): BinaryArray =
    BinaryArray(this.arr.updated(index, elem))
  def toArray = this.arr
}

final case object BinaryArray {
  def apply(x: Short, xs: Short*): BinaryArray = BinaryArray(x +: xs)
  def fill(n: Int)(x: => Short) = BinaryArray(Array.fill(n)(x))
}
