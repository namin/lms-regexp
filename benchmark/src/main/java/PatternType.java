public enum PatternType {
    AAB_AAB ("AAB", "AAB"),
    AB_AAB  ("AB",  "AAB"),
    A_B     ("A",   "B"),
    C_AAB   ("C",   "AAB");

    public final String rep;
    public final String end;
    PatternType(String rep, String end) {
	this.rep = rep;
	this.end = end;
    }
}