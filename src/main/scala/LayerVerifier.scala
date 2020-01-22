class LayerVerifier {
  def doCheck(imageData : String, size : (Int, Int)) : Int = {
    val layerWithLeastZeros = imageData grouped(size._1 * size._2) minBy (x => x.count(_ == '0'))
    layerWithLeastZeros.count(_ == '1') * layerWithLeastZeros.count(_ == '2')
  }
}
