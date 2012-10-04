import RegexpType._

class PMatcherLMS() {
  private val fc = new PMatch1

  def matches(input: String): Boolean = {
    fc(input.toList) != null
  }
}
