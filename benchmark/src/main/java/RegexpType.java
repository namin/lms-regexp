public enum RegexpType {
    ANY_AAB (".*AAB"),
    ANY_AAB_ANY (".*AAB.*"),
    USD ("usd [+-]?[0-9]+.[0-9][0-9]"),
    ANY (".*A?.*");

    public final String re;
    RegexpType(String re) {
	this.re = re;
    }
}