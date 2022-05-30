import java.lang.invoke.StringConcatFactory;

public class test {
    public static void main(String[] args) {
        test t = new test();

        test.hi(t);
        return;
    }

    public static String hi(test t) {
        return test.hi(t);
    }

    public String hey(String x, String y, String z) {
        return x + y + z;
    }
}