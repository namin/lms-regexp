class PMatcherLMS() extends PMatcher {
  private val fc = new PMatch1

  def matches(input: String): Boolean = {
    fc(input.toList) != null
  }
}
