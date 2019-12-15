object DayFour {
  def isPotentialPassword(input: String, partTwoCriteria: Boolean = false) : Boolean = {
    val stringSeq = input.toSeq
    if (stringSeq.sorted != stringSeq) return false

    val frequency = stringSeq.groupBy(identity).map(t => (t._1, t._2.length))

    if (partTwoCriteria) frequency.values.exists(x => x == 2) else frequency.values.exists(x => x >= 2)
  }
}
