public enum MatcherType {
    LMS {
        @Override public RegexpMatcher create(RegexpType regexp) {
            return new MatcherLMS(regexp);
        }
    },
    DK {
        @Override public RegexpMatcher create(RegexpType regexp) {
            return new MatcherDK(regexp);
        }
    },
    JAVA {
        @Override public RegexpMatcher create(RegexpType regexp) {
            return new MatcherJava(regexp);
        }
    };

    public abstract RegexpMatcher create(RegexpType regexp);
}
