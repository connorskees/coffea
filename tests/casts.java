public class casts {

    public static void main(String[] args) {
        int i = 1;
        float f = 2;
        double d = 3;
        long l = 4;

        int f_to_i = (int) f;
        int d_to_i = (int) d;
        int l_to_i = (int) l;

        float i_to_f = (float) i;
        float d_to_f = (float) d;
        float l_to_f = (float) l;

        double i_to_d = (double) i;
        double f_to_d = (double) f;
        double l_to_d = (double) l;

        long i_to_l = (long) i;
        long f_to_l = (long) f;
        long d_to_l = (long) d;

        byte b = (byte) i;
        char c = (char) i;
        short s = (short) i;

        // these are implicitly converted to ints and use i2d
        double b_to_d = (double) b;
        double c_to_d = (double) c;
        double s_to_d = (double) s;
    }
}