public abstract class RegexpMatcher {
    final RegexpType regexp;
    public RegexpMatcher(RegexpType regexp) {
        this.regexp = regexp;
    }
    public abstract boolean matches(String input);
}
