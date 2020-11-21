public class arrays {

    public static void main(String[] args) {
        int a = 1;
        int[] b = new int[a + 5];
        String[] c = new String[a + a];
        String[] c2 = { "hi", "bye" };

        int[] d = { 1, 2, 3 };
        d[2] = 4;
        int e = d[0];
        int[][] f = { { 1, 2 }, { 3, 4 }, { 5, 6 } };
        int[] g = f[1];
        int h = f[0][1];
        int i = f[f[0][0]][d[h + 1]];
    }
}