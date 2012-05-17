public enum InputType {
    AAB ("AAB"),
    X_AAB ("hello AAB"),
    AAB_X ("AAB no"),
    AAB_GARBAGE ("AAB " + garbage(10)),
    USD ("usd 1234.00"),
    GARBAGE (garbage(20));

    public final String text;
    InputType(String text) {
	this.text = text;
    }

    private static String garbage(int n) {
	return new String(new char[n]).replace("\0", "no no no");
    }
}