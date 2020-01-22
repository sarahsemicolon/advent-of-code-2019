class LayeredImageProcessor {
  def doCheck(imageData : String, size : (Int, Int)) : Int = {
    val layerWithLeastZeros = imageData grouped(size._1 * size._2) minBy (x => x.count(_ == '0'))
    layerWithLeastZeros.count(_ == '1') * layerWithLeastZeros.count(_ == '2')
  }

  def decodeImage(imageData: String, size: (Int, Int)) : String = {
    val pixelLists = imageData.grouped (size._1 * size._2).map(x => x.toList).toList.transpose
    pixelLists.map(x => x.find(i => i != '2')).flatten.mkString.grouped(size._1).mkString("\n")
  }
}
