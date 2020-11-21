public class instance_variables {

    public static final int a = 2;
    int b = this.a * 2;
    String c = "hiii";

    public void main(String[] args) {
        this.b = 3;
        String d = this.c;
    }
}
