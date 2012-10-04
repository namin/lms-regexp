public enum PMatcherType {
    LMS {
	@Override public PMatcher create() {
	    return new PMatcherLMS();
	}
    },
    RHINO {
	@Override public PMatcher create() {
	    return new PMatcherRhino();
	}
    };

    public abstract PMatcher create();
}