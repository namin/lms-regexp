public enum RegexpType {
    ANY_AAB (".*AAB"),
    ANY_AAB_ANY (".*AAB.*"),
    USD ("usd [+-]?[0-9]+.[0-9][0-9]"),
    COOK ("(A|B)*((ABB)|(A|B))"),
    ANY (".*A?.*");

    public final String re;
    RegexpType(String re) {
	this.re = re;
    }
}