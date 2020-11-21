public class string_concat_factory {

    public static void main(String[] args) {}

    public String stringConcatFactory(String x, String y, String z) {
        int a = 2;
        int b = 2;

        String s1 = "foo";
        String s2 = "bar" + 2 + a + 1 + a + 1;
        String s3 = s1 + s2 + x;

        return "";
    }
}