object CartesianProduct {

  def main(args: Array[String]): Unit = {
    var sequence = Seq(Seq(1, 2, 3), Seq(4, 5), Seq(6, 7))
    println(enumerate(sequence))
  }

  def enumerate[T](sequences: Seq[Seq[T]]): Seq[Seq[T]] = {

    def productInner(streams: Seq[Seq[T]]): Seq[Seq[T]] = streams match {
      case Seq(first) => {
        for (a <- first)
         yield Seq(a)
      }
      case Seq(first, rest @_*) => {
        for (a <- first; b <- productInner(rest))
         yield a +: b
     }
    }

    def productInnerWithFlatMap(streams: Seq[Seq[T]]): Seq[Seq[T]] = streams match {
      case Seq() => Seq(Seq())
      case Seq(first, rest @_*) => {
        val rest_product: Seq[Seq[T]] = productInnerWithFlatMap(rest);
        first.flatMap((a: T) => rest_product.map(a +: _))
      }
    }
    
    productInnerWithFlatMap(sequences)
  }
}
